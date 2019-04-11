{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent   (forkIO, forkOS)
import           Control.Monad        (forever, unless)
import           Control.Monad.Trans  (liftIO)
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
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
            liftIO $ T.putStrLn msg
            loop
    loop


    liftIO $ T.putStrLn "before closing"
    WS.sendClose conn ("Bye!" :: Text)


main :: IO ()
-- main = withSocketsDo $ WS.runClient "echo.websocket.org" 80 "/" app

main = withSocketsDo $ WS.runClient "localhost" 8080 "/client/ws/speech?content-type=audio/x-raw,+layout=(String)interleaved,+rate=(Int)16000,+format=(String)S16LE,+channels=(Int)1" app

-- main = withSocketsDo $ WS.runClient "localhost" 8080 statusURI app

-- Python client cmd
-- python2 kaldigstserver/client.py -u ws://localhost:8080/client/ws/speech -r 32000 /tmp/1272-128104-0000.wav
