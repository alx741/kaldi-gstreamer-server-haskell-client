{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Concurrent     (forkIO)
import           Control.Monad.Trans    (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text              (Text, pack)
import qualified Data.Text.IO           as T
import           Network.Socket         (withSocketsDo)
import qualified Network.WebSockets     as WS
import           System.Console.CmdArgs
import           System.IO              (stdin)

import Client
import Types


app :: FilePath -> WS.ClientApp ()
app fp conn = do
    putStrLn "Connected!"

    -- Read raw binary from file
    _ <- forkIO $ do

        raw <- liftIO $ LBS.readFile fp
        WS.sendBinaryData conn raw

-- arecord -f S16_LE -r 16000 | stack exec client

        -- raw <- liftIO $ LBS.getContents
        -- WS.sendBinaryData conn raw

        -- raw <- liftIO $ LBS.hGet stdin 204800
        -- WS.sendBinaryData conn raw

        WS.sendTextData conn (pack "EOS" :: Text)

    -- Recive answers
    let loop = do
            msg <- WS.receiveData conn
            let mval = ((decode msg) :: Maybe TranscriptResponse)
            liftIO $ print msg
            liftIO $ print mval
            loop
    loop


    liftIO $ T.putStrLn $ pack "before closing"
    WS.sendClose conn (pack "Bye!" :: Text)


main :: IO ()
main = print =<< cmdArgs clientArgs

-- main = withSocketsDo $ WS.runClient "echo.websocket.org" 80 "/" app

-- main = withSocketsDo $ WS.runClient defaultHost defaultPort "/client/ws/speech?content-type=audio/x-raw,+layout=(String)interleaved,+rate=(Int)44100,+format=(String)S16LE,+channels=(Int)1" (app "some.wav")

-- main = withSocketsDo $ WS.runClient "localhost" 8080 statusURI app

-- Python client cmd
-- python2 kaldigstserver/client.py -u ws://localhost:8080/client/ws/speech -r 32000 /tmp/1272-128104-0000.wav

data Kaldigsclient = Kaldigsclient
    { host :: String
    , port :: Int
    , file :: FilePath
    } deriving (Show, Data, Typeable)

-- clientArgs :: Kaldigsclient
clientArgs = Kaldigsclient
    { host = defaultHost &= help "Kaldi gstreamer server host"
    , port = defaultPort &= help "Kaldi gstreamer server port"
    , file = def &= help "audio file"
    } &= summary "kaldigsclient v0.1.0.0 (C) Daniel Campoverde [alx741] 2019"
    where
        defaultHost = "localhost"
        defaultPort = 8080
