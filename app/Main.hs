{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import SentimentAnalyzer (sentimentAnalyzer, naiveBayesClassifier, preprocessTrainingData)
import qualified Data.Text.Lazy as TL
import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv (decode, HasHeader(NoHeader))
import Text.Read (readMaybe)

type TextSentiment = (Int, String)

main :: IO ()
main = do
    sentimentAnalyzer
    trainVector <- loadTrainingData "updated_training.csv"
    let classFreqs = preprocessTrainingData trainVector

    port <- fmap (fromMaybe 8080 . (>>= readMaybe)) (lookupEnv "PORT")

    scotty port $ do
        get "/" $ do
            text <- param "text"
            let sentiment = if naiveBayesClassifier (TL.unpack text) classFreqs == 4 then TL.pack "positive" else TL.pack "negative"
            json $ object ["text" .= text, "sentiment" .= sentiment]

loadTrainingData :: FilePath -> IO (V.Vector TextSentiment)
loadTrainingData filePath = do
    csvData <- BL.readFile filePath
    case decode NoHeader csvData of
        Left err -> error err
        Right v -> return v
