{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent   (forkIO, forkOS)
import           Control.Monad        (forever, unless, forM)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson
import Data.Scientific
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import qualified Data.Text            as T
import Data.Vector (toList)
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

-- 0d05ef0\"}"
-- "{\"status\": 0, \"segment\": 0, \"result\": {\"hypotheses\": [{\"transcript\": \"AS TO.\"}], \"final\": false}, \"id\": \"51695045-32a7-437f-a108-e71700d05ef0\"}"
-- "{\"status\": 0, \"segment\": 0, \"result\": {\"hypotheses\": [{\"transcript\": \"AS TO QUARTERS.\"}], \"final\": false}, \"id\": \"51695045-32a7-437f-a108-e71700d05ef0\"}"
-- "{\"status\": 0, \"segment\": 0, \"result\": {\"hypotheses\": [{\"transcript\": \"AS TO QUARTERS THE.\"}], \"final\": false}, \"id\": \"51695045-32a7-437f-a108-e71700d05ef0\"}"
-- "{\"status\": 0, \"segment\": 0, \"result\": {\"hypotheses\": [{\"transcript\": \"AS TO QUARTERS THE APOSTLE.\"}], \"final\": false}, \"id\": \"51695045-32a7-437f-a108-e71700d05ef0\"}"
-- "{\"status\": 0, \"segment\": 0, \"result\": {\"hypotheses\": [{\"transcript\": \"AS TO QUARTERS THE APOSTLE OF.\"}], \"final\": false}, \"id\": \"51695045-32a7-437f-a108-e71700d05ef0\"}"



    -- status -- response status (integer), see codes below
    -- message -- (optional) status message
    -- result -- (optional) recognition result, containing the following fields:
    --     hypotheses - recognized words, a list with each item containing the following:
    --         transcript -- recognized words
    --         confidence -- (optional) confidence of the hypothesis (float, 0..1)
    --     final -- true when the hypothesis is final, i.e., doesn't change any more


data TranscriptStatus
    = TransStatusSuccess
    | TransStatusAborted
    | TransStatusNoSpeech
    | TransStatusNotAvailable
    deriving (Show, Generic)

data TranscriptResponse = TranscriptResponse
    { status  :: TranscriptStatus
    , message :: Maybe Text
    , result  :: Maybe TranscriptResult
    } deriving (Show, Generic)

data TranscriptResult = TranscriptResult
    { hypotheses :: [(Transcript, Maybe Confidence)]
    , final      :: Bool
    } deriving (Show, Generic)

type Confidence = Float
type Transcript = Text


instance FromJSON TranscriptResult where
    parseJSON = withObject "result" $ \o -> do
        hypothesesArray <- o .: "hypotheses"
        hypotheses  <- forM (hypothesesArray :: Array) hypo
        final  <- o .: "final"
        pure $ TranscriptResult (toList hypotheses) final
        where
            hypo :: Value -> Parser (Transcript, Maybe Confidence)
            hypo = withObject "hypo" $ \o  -> do
                transcript   <- o .: "transcript"
                confidence   <- o .:? "confidence"
                pure (transcript, confidence)

instance FromJSON TranscriptStatus where
    parseJSON = withScientific "status" $ \n -> pure $ (statusCode2TranscriptStatus . truncate) n


somejson = "{\"status\": 0, \"segment\": 0, \"result\": {\"hypotheses\": [{\"transcript\": \"AS TO QUARTERS THE APOSTLE OF THE MIDDLE CLASSES AND WE'RE GLAD TO WELCOME HIS GOSPEL.\"}], \"final\": false}, \"id\": \"10766ae3-00a0-4431-ab48-f8dd1c533e45\"}"

instance FromJSON TranscriptResponse where
    -- parseJSON = withObject "TranscriptResponse" $ \v -> TranscriptResponse
    parseJSON = withObject "TranscriptResponse" $ \o -> do
        status  <- (o .: "status")
        message  <- (o .:? "message")
        result  <- (o .:? "result")
    --     result <- (o .: "result")
    --     hypotheses <- (result .: "hypotheses")
        pure $ TranscriptResponse status message result


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
