{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent   (forkIO, forkOS)
import           Control.Monad        (forever, unless)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           GHC.Generics
import           Network.Socket       (withSocketsDo)
import qualified Network.WebSockets   as WS
import           System.IO            (hFlush, stdout)

host :: Text
host = "localhost"

port :: Text
port = "8080"

speechURI :: Text
speechURI = "/client/ws/speech"

statusURI :: Text
statusURI = "/client/ws/status"

-- serviceURL :: Text
-- serviceURL = host <> ":" <> port <> "/" <> speechURI


data TranscriptStatus
    = TransStatusSuccess
    | TransStatusAborted
    | TransStatusNoSpeech
    | TransStatusNotAvailable
    deriving (Show, Generic)

data TranscriptResponse = TranscriptResponse
    { status     :: TranscriptStatus
    , hypotheses :: [Transcript]
    , isFinal    :: Bool
    } deriving (Show, Generic)

data Transcript = Transcript Text deriving (Show)

instance FromJSON Transcript where
    parseJSON = withObject "transcript" $ \o -> Transcript <$> o .: "transcript"

somejson = "{\"status\": 0, \"segment\": 0, \"result\": {\"hypotheses\": [{\"transcript\": \"AS TO QUARTERS THE APOSTLE OF THE MIDDLE CLASSES AND WE'RE GLAD TO WELCOME HIS GOSPEL.\"}], \"final\": false}, \"id\": \"10766ae3-00a0-4431-ab48-f8dd1c533e45\"}"

instance FromJSON TranscriptResponse where
    -- parseJSON = withObject "TranscriptResponse" $ \v -> TranscriptResponse
    parseJSON = withObject "TranscriptResponse" $ \o -> do
        status  <- statusCode2TranscriptStatus <$> (o .: "status")
        -- hypothesesO <- o .: "hypotheses"
        result <- (o .: "result")
        hypotheses <- (result .: "hypotheses")
        pure $ TranscriptResponse status hypotheses False


    --     let stat = () .: "status"
        -- <$> statusCode2TranscriptStatus <$> (v .: "status")
        -- -- <*> hypoParser .: "result"
        -- <*> pure "this"
        -- <*> pure False
-- (.:) :: FromJSON a => Object -> Text -> Parser a 
-- withObject :: String -> (Object -> Parser a) -> Value -> Parser a 

hypoParser :: Value -> Parser Text
-- hypoParser = withObject "result" $ \v -> v .: "hypotheses"
hypoParser (Object o) = o .: "hypotheses"

statusCode2TranscriptStatus :: Int -> TranscriptStatus
statusCode2TranscriptStatus n
    | n == 0     = TransStatusSuccess
    | n == 1     = TransStatusNoSpeech
    | n == 2     = TransStatusAborted
    | n == 9     = TransStatusNotAvailable
    | otherwise  = TransStatusAborted
-- instance FromJSON TranscriptResponse

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Read raw binary from file
    _ <- forkIO $ do
        raw <- liftIO $ LBS.readFile "test.wav"
        WS.sendBinaryData conn raw
        WS.sendTextData conn ("EOS" :: Text)

    -- Recive answers
    let loop = do
            msg <- WS.receiveData conn
            let mval = ((decode msg) :: Maybe TranscriptResponse)
            liftIO $ print msg
            liftIO $ print mval
            loop
    loop


    liftIO $ T.putStrLn "before closing"
    WS.sendClose conn ("Bye!" :: Text)


main :: IO ()
main = do
 let eDec = ((eitherDecode somejson) :: Either String TranscriptResponse)
 case eDec of
    Right response -> print response
    Left err -> error err
-- main = withSocketsDo $ WS.runClient "echo.websocket.org" 80 "/" app

-- main = withSocketsDo $ WS.runClient "localhost" 8080 "/client/ws/speech?content-type=audio/x-raw,+layout=(String)interleaved,+rate=(Int)16000,+format=(String)S16LE,+channels=(Int)1" app

-- main = withSocketsDo $ WS.runClient "localhost" 8080 statusURI app

-- Python client cmd
-- python2 kaldigstserver/client.py -u ws://localhost:8080/client/ws/speech -r 32000 /tmp/1272-128104-0000.wav
