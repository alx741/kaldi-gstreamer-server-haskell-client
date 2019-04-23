{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Control.Concurrent   (forkIO)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text, pack, unpack)
import qualified Network.WebSockets   as WS

import Types

speechURI :: Text
speechURI = "/client/ws/speech"

statusURI :: Text
statusURI = "/client/ws/status"

uri :: String
uri = "/client/ws/speech?content-type=audio/x-raw,+layout=(String)interleaved,+rate=(Int)44100,+format=(String)S16LE,+channels=(Int)1"

run :: KaldigsclientArgs -> IO ()
run (KaldigsclientArgs h p f) = WS.runClient h p uri $ app f

app :: FilePath -> WS.ClientApp ()
app fp conn = do
    putStrLn "* Connected to server"

    -- Read raw binary from file
    _ <- forkIO $ do

        putStrLn "* Sending audio data"
        raw <- liftIO $ LBS.readFile fp
        WS.sendBinaryData conn raw

        -- arecord -f S16_LE -r 16000 | stack exec client
        -- raw <- liftIO $ LBS.hGet stdin 204800
        -- WS.sendBinaryData conn raw

        WS.sendTextData conn (pack "EOS" :: Text)

    -- Recive answers
    let loop = do
            rawResponse <- WS.receiveData conn
            let mTranscriptRes = ((decode rawResponse) :: Maybe TranscriptResponse)
            let mTranscript = mTranscriptRes >>=  prettyTranscript
            case mTranscript of
                Just transcript -> putStrLn $ unpack transcript
                Nothing         -> pure ()
            loop
    loop
    -- WS.sendClose conn (pack "Bye!" :: Text)


prettyTranscript :: TranscriptResponse -> Maybe Text
prettyTranscript (TranscriptResponse _ _ (Just (TranscriptResult (h:_) _))) = Just $ fst h
prettyTranscript _ = Nothing
