{-# LANGUAGE OverloadedStrings #-}

module Text.Decker.Internal.PdfExport where

import Control.Concurrent (forkIO)
import Control.Lens ((&), (.~), (^.), (^?))
import Control.Monad (forever, unless, when)
import Control.Monad.Trans (liftIO)
import Data.Aeson as AE
import Data.Aeson.Lens (key, _String)
import Data.Aeson.Types
import Data.ByteString.Base64 as B64
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.Maybe
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Data.Vector (singleton)
import Network.Socket (withSocketsDo)
import Network.WebSockets qualified as WS
import Network.Wreq

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
waitForMessage :: (WS.WebSocketsData a) => (a -> Maybe b) -> WS.ClientApp b
waitForMessage check conn = do
    msg <- WS.receiveData conn
    case check msg of
        Nothing -> waitForMessage check conn
        Just x -> return x

waitForMessageEither :: (WS.WebSocketsData a) => (a -> c -> Either b c) -> c -> WS.ClientApp b
waitForMessageEither check last conn = do
    msg <- WS.receiveData conn
    case check msg last of
        Left x -> return x
        Right x -> waitForMessageEither check x conn

checkMessage :: ByteString -> Maybe ByteString
checkMessage message = if message == "Hi" then Just "Hi" else Nothing

runSocket :: IO ()
runSocket = withSocketsDo $ WS.runClient "127.0.0.1" 8888 "/" (app checkMessage)

exportPdf :: String -> String -> String -> Int -> IO ()
exportPdf url out chromeHost chromePort = do
    putStrLn $ "Creating new tab for \"" ++ url ++ "\""
    creationResponse <- put ("http://" ++ chromeHost ++ ":" ++ show chromePort ++ "/json/new?" ++ url) (toJSON (object []))
    let id = creationResponse ^? responseBody . key "id" . _String <&> Data.Text.unpack
    liftIO $ putStrLn $ "Recieved response for url creation: " ++ show id
    case id of
        Just x -> do
            get ("http://" ++ chromeHost ++ ":" ++ show chromePort ++ "/json/activate/" ++ x)
            liftIO $ putStrLn $ "Connecting to WS at " ++ chromeHost ++ ":" ++ show chromePort ++ "/devtools/page/" ++ x
            pdfData <- withSocketsDo $ WS.runClient chromeHost chromePort ("/devtools/page/" ++ x) exportPdfWebsocketHandler
            case pdfData of
                Just pdfData -> case B64.decode (BS8.pack pdfData) of
                    Left x -> do
                        writeFile out x
                    Right x -> return ()
                Nothing -> return ()
        Nothing -> return ()

exportPdfWebsocketHandler :: WS.ClientApp (Maybe String)
exportPdfWebsocketHandler conn = do
    liftIO $ putStrLn "Connection established!"
    let mId = 1
    connectionData <- establishConnection mId conn
    case connectionData of
        Just (sessionId, mId) -> do
            mId <- waitForPdfReady sessionId mId conn
            websocketRequestPdf sessionId mId conn
        _ -> return Nothing

establishConnection :: Int -> WS.ClientApp (Maybe (String, Int))
establishConnection mId conn = do
    let discover = AE.encode $ object ["method" .= ("Target.setDiscoverTargets" :: String), "params" .= object ["discover" .= True, "filter" .= Array (singleton (object []))], "id" .= mId]
    WS.sendTextData conn discover

    waitForMessage (messageResponse mId) conn
    let mId = mId + 1

    let discover = AE.encode $ object ["method" .= ("Target.setAutoAttach" :: String), "params" .= object ["waitForDebuggerOnStart" .= True, "flatten" .= True, "autoAttach" .= True, "filter" .= Array (singleton (object ["type" .= ("page" :: String), "exclude" .= True]))], "id" .= mId]
    WS.sendTextData conn discover

    response <- waitForMessageEither (getSessionIdWhileWaiting mId) Nothing conn
    let mId = mId + 1

    case response of
        Just sessionId -> return $ Just (sessionId, mId)
        _ -> return Nothing
  where
    getSessionIdWhileWaiting :: Int -> ByteString -> Maybe String -> Either (Maybe String) (Maybe String)
    getSessionIdWhileWaiting mId recievedData savedSessionId = do
        let decodedData = AE.decode recievedData :: Maybe Object
        case decodedData of
            Nothing -> Right savedSessionId
            Just decodedData -> do
                let sessionId = parseMaybe getSessionIdFromJsonMessage decodedData :: Maybe (Maybe String)
                case sessionId of
                    Just (Just sessionId) -> Right $ Just sessionId
                    _ -> do
                        let decodedData = AE.decode recievedData
                        case decodedData of
                            Nothing -> Right savedSessionId
                            Just decodedData -> do
                                let myId = parseMaybe getResultId decodedData
                                case myId of
                                    Just (Just mId) -> Left savedSessionId
                                    _ -> Right savedSessionId

getResultId :: Object -> Parser (Maybe String)
getResultId obj = obj .: "id"

getSessionIdFromJsonMessage :: Object -> Parser (Maybe String)
getSessionIdFromJsonMessage obj = do
    method <- obj .: "method" :: Parser String
    case method of
        "Target.attachedToTarget" -> do
            params <- obj .: "params"
            sessionId <- params .: "sessionId"
            return $ Just sessionId
        _ -> return Nothing

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

waitForPdfReady :: String -> Int -> WS.ClientApp Int
waitForPdfReady sessionId requestCounter conn = do
    let jsWaitForReadyFunction :: String = "() => { return new Promise((resolve) => { const reveal = document.querySelector(\".reveal\"); reveal.addEventListener(\"pdf-ready\", () => { resolve(); }); }); }"
    let jsonMessage = AE.encode (object ["method" .= ("Runtime.callFunctionOn" :: String), "params" .= object ["functionDeclaration" .= jsWaitForReadyFunction, "executionContextId" .= Number 3, "arguments" .= object [], "returnByValue" .= True, "awaitPromise" .= True, "userGesture" .= True], "id" .= requestCounter, "sessionId" .= sessionId])
    WS.sendTextData conn jsonMessage

    waitForMessage (messageResponse requestCounter) conn

    return $ requestCounter + 1

pdfResponseDataParser :: Object -> Parser String
pdfResponseDataParser obj = obj .: "stream"

pdfResponsePdfParser :: Object -> Parser String
pdfResponsePdfParser obj = obj .: "data"

websocketRequestPdf :: String -> Int -> WS.ClientApp (Maybe String)
websocketRequestPdf sessionId requestCounter conn = do
    let pagePrintPdf = AE.encode (object ["method" .= ("Page.printToPDF" :: String), "params" .= object ["transferMode" .= ("ReturnAsStream" :: String), "landscape" .= False, "displayHeaderFooter" .= False, "headerTemplate" .= ("" :: String), "footerTemplate" .= ("" :: String), "printBackground" .= False, "scale" .= Number 1, "paperWidth" .= Number 8.5, "paperHeight" .= Number 11, "marginTop" .= Number 0, "marginBottom" .= Number 0, "marginLeft" .= Number 0, "marginRight" .= Number 0, "pageRanges" .= ("" :: String), "preferCSSPageSize" .= True, "generateTaggedPDF" .= True, "generateDocumentOutline" .= False], "id" .= requestCounter, "sessionId" .= sessionId])
    WS.sendTextData conn pagePrintPdf

    pdfResponse <- waitForMessage (messageResponse requestCounter) conn
    let streamId = parseMaybe pdfResponseDataParser $ result pdfResponse
    case streamId of
        Just sId -> do
            let pageGetPdf = AE.encode (object ["method" .= ("IO.read" :: String), "params" .= object ["handle" .= sId], "id" .= (requestCounter + 1), "sessionId" .= sessionId])
            WS.sendTextData conn pageGetPdf

            pdfResponse <- waitForMessage (messageResponse $ requestCounter + 1) conn
            let pdfData = parseMaybe pdfResponsePdfParser (result pdfResponse)
            return pdfData
        Nothing -> return Nothing

messageResponse :: Int -> ByteString -> Maybe WebsocketResult
messageResponse requestId message = do
    let result = AE.decode message :: Maybe WebsocketResult
    case result of
        Just x -> if mId x == requestId then Just x else Nothing
        Nothing -> Nothing
