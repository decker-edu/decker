{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.PdfExport where

import Control.Concurrent (MVar, ThreadId, forkIO, putMVar, takeMVar)
import Control.Lens ((&), (.~), (^.), (^?))
import Control.Monad (forever, join, unless, when)
import Control.Monad.Trans (liftIO)
import Data.Aeson as AE
import Data.Aeson.KeyMap (toList)
import Data.Aeson.Lens (key, _String)
import Data.Aeson.Types
import Data.ByteString (writeFile)
import Data.ByteString.Base64 as B64
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Data.Vector (singleton)
import Network.Socket (withSocketsDo)
import Network.WebSockets qualified as WS
import Network.Wreq
import Prelude (Bool (..), Either (..), IO, Int, Maybe (..), Show, String, print, putStrLn, return, show, ($), (+), (++), (-), (.), (/=), (==), (>>))

app :: (ByteString -> Maybe ByteString) -> WS.ClientApp ()
app check conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- waitForMessage check conn
        liftIO $ T.putStrLn "Recieved a new message: "
        when (isNothing (check msg)) $ liftIO $ T.putStrLn $ T.pack $ show msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)

-- This listens to a websocket recieving all messages until a message fullfills the check.
-- Then it returns the data calculated by the check function
waitForMessage :: (WS.WebSocketsData a, Show a) => (a -> Maybe b) -> WS.ClientApp b
waitForMessage check conn = do
    msg <- WS.receiveData conn
    -- liftIO $ print msg
    case check msg of
        Nothing -> waitForMessage check conn
        Just x -> return x

waitForMessageEither :: (WS.WebSocketsData a, Show a) => (a -> c -> Either b c) -> c -> WS.ClientApp b
waitForMessageEither check last conn = do
    msg <- WS.receiveData conn
    liftIO $ print msg
    case check msg last of
        Left x -> return x
        Right x -> waitForMessageEither check x conn

checkMessage :: ByteString -> Maybe ByteString
checkMessage message = if message == "Hi" then Just "Hi" else Nothing

runSocket :: IO ()
runSocket = withSocketsDo $ WS.runClient "127.0.0.1" 8888 "/" (app checkMessage)

exportPdf :: String -> String -> String -> Int -> MVar ThreadId -> IO ()
exportPdf url out chromeHost chromePort websocketLock = do
    chromeId <- takeMVar websocketLock
    putStrLn $ "Creating new tab for \"" ++ url ++ "\""
    creationResponse <- put ("http://" ++ chromeHost ++ ":" ++ show chromePort ++ "/json/new?" ++ url) (toJSON (object []))
    let id = creationResponse ^? responseBody . key "id" . _String <&> Data.Text.unpack
    liftIO $ putStrLn $ "Recieved response for url creation: " ++ show id
    case id of
        Just x -> do
            get ("http://" ++ chromeHost ++ ":" ++ show chromePort ++ "/json/activate/" ++ x)

            liftIO $ putStrLn $ "Connecting to WS at " ++ chromeHost ++ ":" ++ show chromePort ++ "/devtools/page/" ++ x
            pdfData <- withSocketsDo $ WS.runClient chromeHost chromePort ("/devtools/page/" ++ x) $ exportPdfWebsocketHandler (url ++ "?print-pdf#/") x
            liftIO $ putStrLn $ "Websocket communication finished for '" ++ url ++ "'!"

            putMVar websocketLock chromeId
            case pdfData of
                Just pdfData -> do
                    let byteData = B64.decodeLenient (BS8.pack pdfData)
                    writeFile out $ byteData
                {-case byteData of
                    Left err -> do
                        liftIO $ putStrLn $ "Invalid data: " ++ err
                    Right outData -> do
                        liftIO $ putStrLn $ "Write PDF to file '" ++ out ++ "'!"
                        writeFile out $ outData-}
                Nothing -> do
                    liftIO $ putStrLn $ "Error while connecting to websocket for url " ++ url ++ "!"
                    return ()
        Nothing -> return ()

exportPdfWebsocketHandler :: String -> String -> WS.ClientApp (Maybe String)
exportPdfWebsocketHandler url targetID conn = do
    liftIO $ putStrLn $ "Websocket connected! " ++ url
    let initMId = 1
    connectionData <- establishConnection targetID initMId conn
    case connectionData of
        Just (sessionId, frameId, mId) -> do
            let mId = 2
            maybeFrameId <- getFrameId url sessionId mId conn
            -- liftIO $ putStrLn $ "Frame ID: " ++ show maybeFrameId
            let mId = 4
            -- case maybeFrameId of
            -- Just (_, _) -> do
            -- maybeExecutionContext <- createExecutionContext frameId sessionId mId conn
            (executionContextId, _) <- navigateToSite frameId url sessionId mId conn
            let mId = 5
            -- case maybeExecutionContext of
            -- Just executionContextId -> do
            waitForPdfReady executionContextId sessionId mId conn
            let mId = 7
            websocketRequestPdf sessionId mId conn

        -- _ -> return Nothing
        -- _ -> return (Nothing)
        _ -> return (Nothing)

establishConnection :: String -> Int -> WS.ClientApp (Maybe (String, String, Int))
establishConnection targetID mId conn = do
    -- let discover = AE.encode $ object ["method" .= ("Target.setDiscoverTargets" :: String), "params" .= object ["discover" .= True, "filter" .= Array (singleton (object []))], "id" .= mId]
    -- WS.sendTextData conn discover

    -- waitForMessage (messageResponse mId) conn
    -- let mId = mId + 1

    let discover = AE.encode $ object ["method" .= ("Target.attachToTarget" :: String), "params" .= object ["targetId" .= targetID, "flatten" .= True], "id" .= mId]
    -- let discover :: ByteString = fromString $ "{\"method\":\"Target.attachToTarget\",\"params\":{\"targetId\":\"" ++ targetID ++ "\",\"flatten\": true},\"id\": " ++ show mId ++ "}"
    WS.sendTextData conn discover

    frameId <- waitForMessage (attachedToTargetExtractFrameId) conn
    sessionId <- waitForMessage (sessionIdResponse mId) conn
    let mId = mId + 1

    liftIO $ putStrLn $ "Got session ID! " ++ sessionId

    return $ Just (sessionId, frameId, mId)
  where
    sessionIdResponse :: Int -> ByteString -> Maybe String
    sessionIdResponse mId recievedData = do
        let decodedData = AE.decode recievedData :: Maybe Object
        case decodedData of
            Just decodedData -> case parseMaybe getResultId decodedData of
                Just (Just id) ->
                    if id == mId
                        then case parseMaybe getSessionIdFromResult decodedData of
                            Just (Just sessionId) -> Just sessionId
                            _ -> Nothing
                        else Nothing
                _ -> Nothing
            _ -> Nothing

getResultId :: Object -> Parser (Maybe Int)
getResultId obj = obj .: "id"

getSessionIdFromResult :: Object -> Parser (Maybe String)
getSessionIdFromResult obj = do
    result <- obj .: "result"
    sessionId <- result .: "sessionId"
    return $ Just sessionId

data WebsocketResult = WebsocketResult
    { mId :: Int
    , result :: Object
    , sessionId :: String
    }
    deriving (Show)

instance FromJSON WebsocketResult where
    parseJSON = withObject "WebsocketResult" decodeWebsocketResult

decodeWebsocketResult :: Object -> Parser WebsocketResult
decodeWebsocketResult obj = do
    mId <- obj .: "id"
    result <- obj .: "result"
    sessionId <- obj .: "sessionId"
    return (WebsocketResult{mId = mId, result = result, sessionId = sessionId})

getFrameId :: String -> String -> Int -> WS.ClientApp (Maybe (Int, String))
getFrameId url sessionId mId conn = do
    let runtimeEnable = AE.encode (object ["method" .= ("Runtime.enable" :: String), "id" .= mId, "sessionId" .= sessionId])
    WS.sendTextData conn runtimeEnable

    {- As at least one execution context should exists as we                -
     - opened this connection on a specific browser page we will always get -
     - at least one Runtime.executionContextCreated message.                -}
    executionContextMessage <- waitForMessage (executionContextCreatedMessage sessionId Nothing) conn

    -- Wait for event to finish
    waitForMessage (messageResponse mId) conn

    {- let jsonMessage = AE.encode (object ["method" .= ("Page.navigate" :: String), "params" .= object ["url" .= url], "id" .= (mId + 1), "sessionId" .= sessionId])

    WS.sendTextData conn jsonMessage

    frameMessage <- waitForMessage (messageResponse (mId + 1)) conn-}

    let enableScriptExecution = AE.encode (object ["method" .= ("Emulation.setScriptExecutionDisabled" :: String), "params" .= object ["value" .= False], "id" .= (mId + 2), "sessionId" .= sessionId])

    WS.sendTextData conn enableScriptExecution

    waitForMessage (messageResponse (mId + 2)) conn
    -- let runIfWaitForDebuggerMessage = AE.encode (object ["method" .= ("Runtime.runIfWaitingForDebugger" :: String), "id" .= (mId + 2), "sessionId" .= sessionId])
    -- WS.sendTextData conn runIfWaitForDebuggerMessage
    -- waitForMessage (messageResponse (mId + 2)) conn

    return $ Just executionContextMessage

navigateToSite :: String -> String -> String -> Int -> WS.ClientApp (Int, String)
navigateToSite frameId url sessionId mId conn = do
    let jsonMessage = AE.encode (object ["method" .= ("Page.navigate" :: String), "params" .= object ["url" .= url, "frameId" .= frameId], "id" .= (mId), "sessionId" .= sessionId])
    liftIO $ putStrLn $ "Prepare page navigation: " ++ show jsonMessage
    WS.sendTextData conn jsonMessage
    {- Every page navigation will create a new executionContextId.          -
     - We therefore get a new Runtime.executionContextCreated event from    -
     - which we can read the new executionContext and frameId.              -}
    waitForMessage (executionContextCreatedMessage sessionId (Just frameId)) conn

createExecutionContext :: String -> String -> Int -> WS.ClientApp (Maybe Int)
createExecutionContext frameId sessionId mId conn = do
    let jsonMessage = AE.encode (object ["method" .= ("Page.createIsolatedWorld" :: String), "params" .= object ["frameId" .= frameId, "worldName" .= ("decker_pdf_exporter" :: String), "grantUniveralAccess" .= True], "id" .= mId, "sessionId" .= sessionId])

    WS.sendTextData conn jsonMessage

    contextCreationResponse <- waitForMessage (messageResponse mId) conn

    liftIO $ print contextCreationResponse
    let contextId = parseMaybe (.: "executionContextId") $ result contextCreationResponse
    return contextId

waitForPdfReady :: Int -> String -> Int -> WS.ClientApp Int
waitForPdfReady executionContextId sessionId requestCounter conn = do
    liftIO $ putStrLn "Waiting for side to become ready for pdf export"
    let jsWaitForReadyFunction :: String = "() => { return new Promise((resolve) => { const reveal = document.querySelector(\".reveal\"); reveal.addEventListener(\"pdf-ready\", () => { resolve(); }); }); }"
    let jsonMessage = AE.encode (object ["method" .= ("Runtime.callFunctionOn" :: String), "params" .= object ["functionDeclaration" .= jsWaitForReadyFunction, "executionContextId" .= executionContextId, "returnByValue" .= True, "awaitPromise" .= True, "userGesture" .= True], "id" .= requestCounter, "sessionId" .= sessionId])

    -- waitForMessage messageReadOne conn

    -- waitForMessage messageReadOne conn

    WS.sendTextData conn jsonMessage

    liftIO $ putStrLn $ "Wait message send, now waiting! " ++ show jsonMessage

    response <- waitForMessage (messageResponse requestCounter) conn

    liftIO $ putStrLn $ "Side ready for pdf export!" ++ show response

    liftIO $ putStrLn "Check fonts ready"

    let jsWaitForFontsFunction :: String = "() => { return document.fonts.ready; }"
    let jsonMessage = AE.encode (object ["method" .= ("Runtime.callFunctionOn" :: String), "params" .= object ["functionDeclaration" .= jsWaitForFontsFunction, "executionContextId" .= executionContextId, "returnByValue" .= True, "awaitPromise" .= True, "userGesture" .= True], "id" .= (requestCounter + 1), "sessionId" .= sessionId])

    WS.sendTextData conn jsonMessage
    response <- waitForMessage (messageResponse (requestCounter + 1)) conn

    liftIO $ putStrLn $ "Fonts ready" ++ show response

    return $ requestCounter + 1

pdfResponseDataParser :: Object -> Parser String
pdfResponseDataParser obj = obj .: "stream"

pdfResponsePdfParser :: Object -> Parser String
pdfResponsePdfParser obj = obj .: "data"

websocketRequestPdf :: String -> Int -> WS.ClientApp (Maybe String)
websocketRequestPdf sessionId requestCounter conn = do
    liftIO $ putStrLn "Requesting PDF"
    let pagePrintPdf = AE.encode (object ["method" .= ("Page.printToPDF" :: String), "params" .= object ["displayHeaderFooter" .= False, "preferCSSPageSize" .= True], "id" .= requestCounter, "sessionId" .= sessionId])
    WS.sendTextData conn pagePrintPdf

    pdfResponse <- waitForMessage (messageResponse requestCounter) conn
    -- liftIO $ putStrLn $ "PDF RESPONSE: " ++ show pdfResponse
    let pdfData = parseMaybe pdfResponsePdfParser $ result pdfResponse
    return pdfData

-- liftIO $ putStrLn $ "PDF ready using streamId: " ++ show streamId

{- case streamId of
    Just sId -> do

        let pageGetPdf = AE.encode (object ["method" .= ("IO.read" :: String), "params" .= object ["handle" .= sId], "id" .= (requestCounter + 1), "sessionId" .= sessionId])
        WS.sendTextData conn pageGetPdf

        pdfResponse <- waitForMessage (messageResponse $ requestCounter + 1) conn
        let pdfData = parseMaybe pdfResponsePdfParser (result pdfResponse)
        return pdfData
    Nothing -> return Nothing -}

getTargetId :: Object -> Parser (Maybe String)
getTargetId obj = do
    params <- obj .: "params"
    targetInfo <- params .: "targetInfo"
    targetInfo .: "targetId"

attachedToTargetExtractFrameId :: ByteString -> Maybe String
attachedToTargetExtractFrameId message = do
    result <- AE.decode message :: Maybe Object
    join $ parseMaybe getTargetId result

getContextAndFrameIdFromContextCreationMessage :: String -> Maybe String -> Object -> Parser (Maybe (Int, String))
getContextAndFrameIdFromContextCreationMessage testSessionId testFrameId obj = do
    sessionId <- obj .: "sessionId"
    if sessionId /= testSessionId
        then return Nothing
        else do
            params <- obj .: "params"
            context <- params .: "context"
            contextId <- context .: "id"
            auxData <- context .: "auxData"
            frameId <- auxData .: "frameId"
            case testFrameId of
                Just fId -> if fId == frameId then return (Just (contextId, frameId)) else return Nothing
                Nothing -> return $ Just (contextId, frameId)

{- Returns Just (executionContextId, frameId), when a Runtime.executionContextCreated message is detected -}
executionContextCreatedMessage :: String -> Maybe String -> ByteString -> Maybe (Int, String)
executionContextCreatedMessage testSessionId frameId message = do
    result <- AE.decode message :: Maybe Object
    join $ parseMaybe (getContextAndFrameIdFromContextCreationMessage testSessionId frameId) result

messageResponse :: Int -> ByteString -> Maybe WebsocketResult
messageResponse requestId message = do
    let result = AE.decode message :: Maybe WebsocketResult
    case result of
        Just x -> if mId x == requestId then Just x else Nothing
        Nothing -> Nothing
