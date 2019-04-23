module Main where

import Data.Semigroup      ((<>))
import Options.Applicative

import Client
import Types

main :: IO ()
main = do
    args <- execParser opts
    run args
    where
        opts = info (kaldigsclientArgsParser <**> helper)
            (  fullDesc
            <> header "Kaldi Gstreamer client v0.1.0.0 (C) Daniel Campoverde 2019"
            <> progDesc "Kaldi Gstreamer client (kaldigsclient) lets you query a \
                        \kaldi-gstreamer-server instance \
                        \(https://github.com/alumae/kaldi-gstreamer-server)"
            )


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
