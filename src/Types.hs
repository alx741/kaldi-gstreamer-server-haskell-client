{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Monad    (forM)
import Data.Aeson
import Data.Aeson.Types
import Data.Text        (Text)
import GHC.Generics

data KaldigsclientArgs = KaldigsclientArgs
    { host :: String
    , port :: Int
    , file :: FilePath
    } deriving (Show)

data TranscriptResponse = TranscriptResponse
    { status  :: TranscriptStatus
    , message :: Maybe Text
    , result  :: Maybe TranscriptResult
    } deriving (Show, Generic)

data TranscriptResult = TranscriptResult
    { hypotheses :: [(Transcript, Maybe Confidence)]
    , final      :: Bool
    } deriving (Show, Generic)

data TranscriptStatus
    = TransStatusSuccess
    | TransStatusAborted
    | TransStatusNoSpeech
    | TransStatusNotAvailable
    deriving (Show, Generic)

type Confidence = Float
type Transcript = Text

instance FromJSON TranscriptResponse where
    parseJSON = withObject "TranscriptResponse" $ \o -> TranscriptResponse
        <$> o .: "status"
        <*> o .:? "message"
        <*> o .:? "result"


instance FromJSON TranscriptStatus where
    parseJSON = withScientific "status" $ \n ->
        pure $ (statusCode2TranscriptStatus . truncate) n
        where
            statusCode2TranscriptStatus :: Int -> TranscriptStatus
            statusCode2TranscriptStatus n
                | n == 0     = TransStatusSuccess
                | n == 1     = TransStatusNoSpeech
                | n == 2     = TransStatusAborted
                | n == 9     = TransStatusNotAvailable
                | otherwise  = TransStatusAborted


instance FromJSON TranscriptResult where
    parseJSON = withObject "result" $ \o -> do
        hypothesesArray <- o .: "hypotheses"
        TranscriptResult
            <$> forM hypothesesArray hypo
            <*> o .: "final"
        where
            hypo :: Value -> Parser (Transcript, Maybe Confidence)
            hypo = withObject "hypo" $ \o -> do
                transcript <- o .: "transcript"
                confidence <- o .:? "confidence"
                pure (transcript, confidence)
