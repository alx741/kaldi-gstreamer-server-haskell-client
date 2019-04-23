{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Concurrent   (forkIO)
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Semigroup       ((<>))
import           Data.Text            (Text, pack)
import qualified Data.Text.IO         as T
import           Network.Socket       (withSocketsDo)
import qualified Network.WebSockets   as WS
import           Options.Applicative
import           System.Exit          (exitFailure)
import           System.IO            (stdin)

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
main = do
    args <- execParser opts
    print args
    where
        opts = info (kaldigsclientArgsParser <**> helper)
            (  fullDesc
            <> header "Kaldi Gstreamer client v0.1.0.0 (C) Daniel Campoverde 2019"
            <> progDesc "Kaldi Gstreamer client (kaldigsclient) lets you query a \
                        \kaldi-gstreamer-server instance \
                        \(https://github.com/alumae/kaldi-gstreamer-server)"
            )

    -- args <- cmdArgs clientArgs
    -- when (file clientArgs == "") $ putStrLn "Empty file argument" >> exitFailure
    -- pure ()

-- main = withSocketsDo $ WS.runClient "echo.websocket.org" 80 "/" app

-- main = withSocketsDo $ WS.runClient defaultHost defaultPort "/client/ws/speech?content-type=audio/x-raw,+layout=(String)interleaved,+rate=(Int)44100,+format=(String)S16LE,+channels=(Int)1" (app "some.wav")

-- main = withSocketsDo $ WS.runClient "localhost" 8080 statusURI app

-- Python client cmd
-- python2 kaldigstserver/client.py -u ws://localhost:8080/client/ws/speech -r 32000 /tmp/1272-128104-0000.wav

data KaldigsclientArgs = KaldigsclientArgs
    { host :: String
    , port :: Int
    , file :: FilePath
    } deriving (Show)

kaldigsclientArgsParser :: Parser KaldigsclientArgs
kaldigsclientArgsParser = KaldigsclientArgs
        <$> strOption
            ( long "host"
            <> short 'h'
            <> metavar "HOST"
            <> help "Kaldi gstreamer server host"
            <> showDefault
            <> value defaultHost
            )
        <*> option auto
            ( long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "Kaldi gstreamer server port"
            <> showDefault
            <> value defaultPort
            )
        <*> strOption
            ( long "file"
            <> short 'f'
            <> metavar "FILE"
            <> help "Audio file"
            )
    where
        defaultHost = "localhost"
        defaultPort = 8080
