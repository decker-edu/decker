{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.PdfExport where

-- import Control.Concurrent (MVar, ThreadId) -- multi-threaded version
import Control.Concurrent (MVar, ThreadId, putMVar, takeMVar) -- single-threaded version

import Control.Lens ((^?))
import Control.Monad (join)
import Control.Monad.Trans (liftIO)
import Data.Aeson as AE
import Data.Aeson.Lens (key, _String)
import Data.Aeson.Types
import Data.ByteString (writeFile)
import Data.ByteString.Base64 as B64
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.Text (unpack)
import Network.Socket (withSocketsDo)
import Network.WebSockets qualified as WS
import Network.Wreq
import Prelude (Bool (..), Either (..), IO, Int, Maybe (..), Show, String, putStrLn, return, show, ($), (&&), (+), (++), (.), (/=), (==))

-- This listens to a websocket recieving all messages until a message fullfills the check.
-- Then it returns the data calculated by the check function
waitForMessage :: (WS.WebSocketsData a, Show a) => (a -> Maybe b) -> WS.ClientApp b
waitForMessage check conn = do
    msg <- WS.receiveData conn
    -- For debugging incomming messages uncommend this line:
    -- liftIO $ print msg
    case check msg of
        Nothing -> waitForMessage check conn
        Just x -> return x

-- This listens to a websocket recieving all messages until a message fullfills (Left) the check.
-- Then it returns the data given by Left. Also gives the check function context passed by (Right), that needs to be initialized.
waitForMessageEither :: (WS.WebSocketsData a, Show a) => (a -> c -> Either b c) -> c -> WS.ClientApp b
waitForMessageEither check last conn = do
    msg <- WS.receiveData conn
    -- For debugging incomming messages uncommend this line:
    -- liftIO $ print msg
    case check msg last of
        Left x -> do
            return x
        Right x -> do
            waitForMessageEither check x conn

-- This method exports the given revealJS page found at baseUrl to a file specified by out.
-- For the exporting a connection to a chrome remote debugging port at chromeHost:chromePort is used.
-- With the websocketLock given to this function multithreading can be disabled while communicating with the chrome instance
-- to allow easier debugging.
--
-- A description of the chrome debugging API can be found here:
-- https://chromedevtools.github.io/devtools-protocol/ or https://github.com/ChromeDevTools/devtools-protocol/?tab=readme-ov-file
exportPdf :: String -> String -> String -> Int -> MVar ThreadId -> IO ()
exportPdf baseUrl out chromeHost chromePort websocketLock = do
    -- Uncommend if you don't want multithreaded pdf loading (note there are two more lines in this method ;)):
    chromeId <- takeMVar websocketLock
    let url = baseUrl ++ "?print-pdf#/"
    putStrLn $ "[PDF-Export] [" ++ url ++ "] Creating new tab!"
    -- Create a new chrome tab with page `about:blank` loaded.
    -- NOTE: You need to load here another page than the one you want to export.
    -- If you don't the site will not be correctly reloaded by chrome later on, so that important events will not be send.
    creationResponse <- put ("http://" ++ chromeHost ++ ":" ++ show chromePort ++ "/json/new?" ++ "about:blank") (toJSON (object []))

    let maybeId = creationResponse ^? responseBody . key "id" . _String <&> Data.Text.unpack
    case maybeId of
        Just id -> do
            get ("http://" ++ chromeHost ++ ":" ++ show chromePort ++ "/json/activate/" ++ id)

            putStrLn $ "[PDF-Export] [" ++ url ++ "] Open websocket connection at '" ++ chromeHost ++ ":" ++ show chromePort ++ "/devtools/page/" ++ id ++ "'!"
            pdfData <- withSocketsDo $ WS.runClient chromeHost chromePort ("/devtools/page/" ++ id) $ exportPdfWebsocketHandler url id
            putStrLn $ "[PDF-Export] [" ++ url ++ "] Closing websocket connection at '" ++ chromeHost ++ ":" ++ show chromePort ++ "/devtools/page/" ++ id ++ "'!"

            get ("http://" ++ chromeHost ++ ":" ++ show chromePort ++ "/json/close/" ++ id)
            -- Uncommend if you don't want multithreaded pdf loading:
            putMVar websocketLock chromeId
            case pdfData of
                Just pdfData -> do
                    putStrLn $ "[PDF-Export] [" ++ url ++ "] Writing PDF to '" ++ out ++ "'!"
                    let byteData = B64.decodeLenient (BS8.pack pdfData)
                    writeFile out byteData
                    putStrLn $ "[PDF-Export] [" ++ url ++ "] Finished PDF export!"
                Nothing -> do
                    liftIO $ putStrLn $ "[PDF-Export] [" ++ url ++ "] Error while connecting to websocket for url " ++ url ++ "!"
                    return ()
        Nothing -> do
            -- Uncommend if you don't want multithreaded pdf loading:
            putMVar websocketLock chromeId
            return ()

-- Chrome remote debugging websocket pdf export connection handling function.
-- This function handles a websocket connection to export the given url using the chrome tab (targetId).
exportPdfWebsocketHandler :: String -> String -> WS.ClientApp (Maybe String)
exportPdfWebsocketHandler url targetID conn = do
    let mId = 1
    connectionData <- establishConnection targetID mId conn -- one message
    putStrLn $ "[PDF-Export] [" ++ url ++ "] Websocket connected to target tab!"
    case connectionData of
        Just (sessionId, frameId) -> do
            let mId = 2
            executionContexts <- setupChromeContext url sessionId mId conn -- three messages
            putStrLn $ "[PDF-Export] [" ++ url ++ "] Chrome executionContexts detected!"
            let mId = 5
            nextExecContext <- navigateToSite frameId url sessionId mId conn -- one message
            putStrLn $ "[PDF-Export] [" ++ url ++ "] Chrome navigated to site!"
            let mId = 6
            nextMessageId <- waitForPdfReady (nextExecContext : executionContexts) sessionId mId conn
            putStrLn $ "[PDF-Export] [" ++ url ++ "] Side ready for pdf export!"
            result <- websocketRequestPdf sessionId nextMessageId conn
            putStrLn $ "[PDF-Export] [" ++ url ++ "] PDF data received!"
            return result
        _ -> return Nothing

-- Attaches this websocket connection to the given tab specified by targetId.
-- Sends only one message to the websocket.
-- Returns the sessionId and frameId.
establishConnection :: String -> Int -> WS.ClientApp (Maybe (String, String))
establishConnection targetID mId conn = do
    let discover = AE.encode $ object ["method" .= ("Target.attachToTarget" :: String), "params" .= object ["targetId" .= targetID, "flatten" .= True], "id" .= mId]
    WS.sendTextData conn discover

    frameId <- waitForMessage attachedToTargetExtractFrameId conn
    sessionId <- waitForMessage (sessionIdResponse mId) conn

    return $ Just (sessionId, frameId)
  where
    -- Waits for a message with the specific message ID and if found returns the sessionId stored in the message
    sessionIdResponse :: Int -> ByteString -> Maybe String
    sessionIdResponse mId recievedData = do
        decodedData <- AE.decode recievedData :: Maybe Object
        id <- join $ parseMaybe getResultId decodedData
        if id == mId
            then join $ parseMaybe getSessionIdFromResult decodedData
            else Nothing

    -- Extracts the targetID from a json object
    getTargetId :: Object -> Parser (Maybe String)
    getTargetId obj = do
        params <- obj .: "params"
        targetInfo <- params .: "targetInfo"
        targetInfo .: "targetId"

    -- Waits for a message that specifies the targetId in a targetInfo parameter
    attachedToTargetExtractFrameId :: ByteString -> Maybe String
    attachedToTargetExtractFrameId message = do
        result <- AE.decode message :: Maybe Object
        join $ parseMaybe getTargetId result

getResultId :: Object -> Parser (Maybe Int)
getResultId obj = obj .: "id"

getSessionIdFromResult :: Object -> Parser (Maybe String)
getSessionIdFromResult obj = do
    result <- obj .: "result"
    sessionId <- result .: "sessionId"
    return $ Just sessionId

-- The general websocket result that is send in response to all requests
data WebsocketResult = WebsocketResult
    { mId :: Int
    , result :: Object
    , sessionId :: String
    }
    deriving (Show)

instance FromJSON WebsocketResult where
    parseJSON = withObject "WebsocketResult" decodeWebsocketResult

-- Parse a WebsocketResult from a json message
decodeWebsocketResult :: Object -> Parser WebsocketResult
decodeWebsocketResult obj = do
    mId <- obj .: "id"
    result <- obj .: "result"
    sessionId <- obj .: "sessionId"
    return (WebsocketResult{mId = mId, result = result, sessionId = sessionId})

-- Setup the browser context and returns a list of all executionContextIds and the corresponding frameIds
-- This makes 3 requests
setupChromeContext :: String -> String -> Int -> WS.ClientApp [(Int, String)]
setupChromeContext url sessionId mId conn = do
    -- Enable runtime event reporting
    let runtimeEnable = AE.encode (object ["method" .= ("Runtime.enable" :: String), "id" .= mId, "sessionId" .= sessionId])
    WS.sendTextData conn runtimeEnable

    -- All existing executionContexts are reported before the message response is send back
    (executionContextMessage, resultMessage) <- waitForMessageEither (executionContextCreatedMessageOnStart mId sessionId) [] conn

    -- Disable browser cache
    let cacheDisable = AE.encode (object ["method" .= ("Network.setCacheDisabled" :: String), "params" .= object ["cacheDisabled" .= True], "id" .= (mId + 1), "sessionId" .= sessionId])
    WS.sendTextData conn cacheDisable
    waitForMessage (messageResponse (mId + 1)) conn

    -- Enables page events reporting
    let pageReportingEnabled = AE.encode (object ["method" .= ("Page.enable" :: String), "id" .= (mId + 2), "sessionId" .= sessionId])
    WS.sendTextData conn pageReportingEnabled
    waitForMessage (messageResponse (mId + 2)) conn

    return executionContextMessage

-- Navigates the given tab (frameId) to the given side (url) and returns the executionContextId and frameId
-- This makes 1 request
navigateToSite :: String -> String -> String -> Int -> WS.ClientApp (Int, String)
navigateToSite frameId url sessionId mId conn = do
    let jsonMessage = AE.encode (object ["method" .= ("Page.navigate" :: String), "params" .= object ["url" .= url, "frameId" .= frameId], "id" .= mId, "sessionId" .= sessionId])
    WS.sendTextData conn jsonMessage
    {- Every page navigation will create a new executionContextId.          -
     - We therefore get a new Runtime.executionContextCreated event from    -
     - which we can read the new executionContext and frameId.              -}
    newContext <- waitForMessage (executionContextCreatedMessage sessionId (Just frameId)) conn

    -- Wait for the page to become loaded.
    -- NOTE: This event is only fired if we navigate from another side to this side.
    -- Therefore this method freezes if called with the url of the side this frame is already on!
    waitForMessage (loadEventFired sessionId) conn
    return newContext

-- Waits for the pdf to become ready to render. This executes a message in all executionContexts and waits for the first function to return.
-- This therefore makes count executionContexts + 1 requests
-- Returns the next message ID.
-- NOTE: This message may freeze on non revealJS websites!
waitForPdfReady :: [(Int, String)] -> String -> Int -> WS.ClientApp Int
waitForPdfReady executionContextId sessionId requestCounter conn = do
    -- Sends a function to be executed to all execution contexts and then waits for the first to finish
    messagesSend <- sendWaitForReadyMessage executionContextId sessionId requestCounter conn
    (expectedId, executionContextId, frameId, message) <- waitForMessage (messageResponses messagesSend) conn

    -- Checks that the fonts are loaded (May not be needed)
    let jsWaitForFontsFunction :: String = "() => { return document.fonts.ready; }"
    let jsonMessage = AE.encode (object ["method" .= ("Runtime.callFunctionOn" :: String), "params" .= object ["functionDeclaration" .= jsWaitForFontsFunction, "executionContextId" .= executionContextId, "returnByValue" .= True, "awaitPromise" .= True, "userGesture" .= True], "id" .= (requestCounter + 1), "sessionId" .= sessionId])
    WS.sendTextData conn jsonMessage
    response <- waitForMessage (messageResponse (requestCounter + 1)) conn

    return $ requestCounter + 2
  where
    sendWaitForReadyMessage :: [(Int, String)] -> String -> Int -> WS.ClientApp [(Int, Int, String)]
    sendWaitForReadyMessage ((executionContextId, frameId) : remainder) sessionId mId conn = do
        let jsWaitForReadyFunction :: String = "() => { return new Promise((resolve) => { let reveal = document.querySelector(\".reveal\"); while(reveal === null) {reveal = document.querySelector(\".reveal\")}; reveal.addEventListener(\"pdf-ready\", () => { resolve(); }); }); }"
        let jsonMessage = AE.encode (object ["method" .= ("Runtime.callFunctionOn" :: String), "params" .= object ["functionDeclaration" .= jsWaitForReadyFunction, "executionContextId" .= executionContextId, "returnByValue" .= True, "awaitPromise" .= True, "userGesture" .= True], "id" .= mId, "sessionId" .= sessionId])

        WS.sendTextData conn jsonMessage

        remainding <- sendWaitForReadyMessage remainder sessionId (mId + 1) conn
        return $ (mId, executionContextId, frameId) : remainding
    sendWaitForReadyMessage _ _ _ _ = return []

pdfResponseDataParser :: Object -> Parser String
pdfResponseDataParser obj = obj .: "stream"

pdfResponsePdfParser :: Object -> Parser String
pdfResponsePdfParser obj = obj .: "data"

-- Requests the pdf and returns it as base64 string
websocketRequestPdf :: String -> Int -> WS.ClientApp (Maybe String)
websocketRequestPdf sessionId requestCounter conn = do
    let pagePrintPdf = AE.encode (object ["method" .= ("Page.printToPDF" :: String), "params" .= object ["displayHeaderFooter" .= False, "preferCSSPageSize" .= True], "id" .= requestCounter, "sessionId" .= sessionId])
    WS.sendTextData conn pagePrintPdf

    pdfResponse <- waitForMessage (messageResponse requestCounter) conn
    let pdfData = parseMaybe pdfResponsePdfParser $ result pdfResponse
    return pdfData

loadEventFired :: String -> ByteString -> Maybe ()
loadEventFired sessionId message = do
    result <- AE.decode message :: Maybe Object
    join $ parseMaybe (checkLoadEventFired sessionId) result
  where
    checkLoadEventFired :: String -> Object -> Parser (Maybe ())
    checkLoadEventFired testSessionId obj = do
        sessionId <- obj .: "sessionId"
        method <- obj .: "method"
        if sessionId == testSessionId && method == ("Page.loadEventFired" :: String)
            then return $ Just ()
            else return Nothing

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

executionContextCreatedMessageOnStart :: Int -> String -> ByteString -> [(Int, String)] -> Either ([(Int, String)], WebsocketResult) [(Int, String)]
executionContextCreatedMessageOnStart messageToWaitFor sessionId message prevData = do
    case AE.decode message :: Maybe Object of
        Just result ->
            case join $ parseMaybe (getContextAndFrameIdFromContextCreationMessage sessionId Nothing) result of
                Just messageData -> Right $ messageData : prevData
                Nothing -> case parseMaybe decodeWebsocketResult result of
                    Just resultData -> if mId resultData == messageToWaitFor then Left (prevData, resultData) else Right prevData
                    Nothing -> Right prevData
        Nothing -> Right prevData

messageResponse :: Int -> ByteString -> Maybe WebsocketResult
messageResponse requestId message = do
    let result = AE.decode message :: Maybe WebsocketResult
    case result of
        Just x -> if mId x == requestId then Just x else Nothing
        Nothing -> Nothing

messageResponses :: [(Int, Int, String)] -> ByteString -> Maybe (Int, Int, String, WebsocketResult)
messageResponses messagesToExpect message = do
    result <- AE.decode message :: Maybe WebsocketResult
    isResultToMessage result messagesToExpect
  where
    isResultToMessage :: WebsocketResult -> [(Int, Int, String)] -> Maybe (Int, Int, String, WebsocketResult)
    isResultToMessage message ((expectedId, executionContextId, frameId) : remainding) =
        if mId message == expectedId
            then Just (expectedId, executionContextId, frameId, message)
            else isResultToMessage message remainding
    isResultToMessage _ _ = Nothing
