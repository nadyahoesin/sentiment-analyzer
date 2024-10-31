module SentimentAnalyzer (main) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv ( decode, HasHeader(HasHeader) )
import qualified Data.Vector as V
import Data.Char (toLower, isAlpha)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type TextSentiment = (String, String)
type WordFreqsByClass = (Map.Map String Int, Map.Map String Int, Map.Map String Int)

main :: IO ()
main = do
    -- Membaca dataset
    csvData <- BL.readFile "chat_dataset.csv"
    case decode HasHeader csvData of
        Left err -> putStrLn err
        Right v -> do
            let (testVector, trainVector) = V.splitAt (V.length v `div` 3) v

            let classFreqs@(posFreq, negFreq, neuFreq) = preprocessTrainingData trainVector
            putStrLn "Positive Token Frequencies:"
            print posFreq
            putStrLn "Negative Token Frequencies:"
            print negFreq
            putStrLn "Neutral Token Frequencies:"
            print neuFreq

            putStrLn "Class Probabilities:"
            print $ computeClassProbs classFreqs

            putStrLn "Probability of token \"wonderful\" given each class:"
            print $ computeTokenGivenClassProbs "wonderful" classFreqs
            
            putStrLn "Probability of token \"terrible\" given each class:"
            print $ computeTokenGivenClassProbs "terrible" classFreqs

            putStrLn "Probability of token \"mediocre\" given each class:"
            print $ computeTokenGivenClassProbs "mediocre" classFreqs


-- Mengubah 1 vektor berisi semua data menjadi tuple berisi map token ke jumlah kemunculan token untuk tiap kelas
preprocessTrainingData :: V.Vector TextSentiment -> WordFreqsByClass
preprocessTrainingData v = (preprocess pos, preprocess neg, preprocess neu)
    where
        (pos, neg, neu) = separateByClass v
        preprocess = countTokensFreq . concatMap tokenize

-- Mengubah 1 vektor berisi semua data menjadi tuple berisi 3 lists
-- list 1: semua teks dengan sentimen positif
-- list 2: semua teks dengan sentimen negatif
-- list 3: semua teks dengan sentimen netral
separateByClass :: V.Vector TextSentiment -> ([String], [String], [String])
separateByClass v = (positive, negative, neutral)
    where
        positive = getTexts $ V.filter (\(_,sentiment) -> sentiment == "positive") v
        negative = getTexts $ V.filter (\(_,sentiment) -> sentiment == "negative") v
        neutral = getTexts $ V.filter (\(_,sentiment) -> sentiment == "neutral") v
        
getTexts :: V.Vector (a, b) -> [a]
getTexts = V.toList . V.map fst

-- Tokenisasi dan normalisasi teks menjadi token
tokenize :: String -> [String]
tokenize = words . map toLower . filter (\c -> isAlpha c || c == ' ')

-- Mengubah list of tokens menjadi map token ke jumlah kemunculan token
countTokensFreq :: [String] -> Map.Map String Int
countTokensFreq = foldr (\token -> Map.insertWith (+) token 1) Map.empty

-- removeUninformativeTokens :: WordFreqsByClass -> WordFreqsByClass

-- Mendapatkan probabilitas tiap kelas
computeClassProbs :: WordFreqsByClass -> (Float, Float, Float)
computeClassProbs (pos, neg, neu) = (numOfPosTokens / numOfAllTokens, 
                                     numOfPosTokens / numOfAllTokens, 
                                     numOfPosTokens / numOfAllTokens)
    where 
        numOfPosTokens = countAllTokens pos
        numOfNegTokens = countAllTokens neg
        numOfNeuTokens = countAllTokens neu
        numOfAllTokens = numOfPosTokens + numOfNegTokens + numOfNeuTokens
        countAllTokens = fromIntegral . Map.foldr (+) 0

-- Mendapatkan probabilitas untuk token given kelas untuk tiap kelas
computeTokenGivenClassProbs :: String -> WordFreqsByClass -> (Float, Float, Float)
computeTokenGivenClassProbs token (pos, neg, neu) = (numOfPos / numOfAll, numOfNeg / numOfAll, numOfNeu / numOfAll)
    where 
        numOfPos = getNumOfToken pos
        numOfNeg = getNumOfToken neg
        numOfNeu = getNumOfToken neu
        numOfAll = numOfPos + numOfNeg + numOfNeu
        getNumOfToken = fromIntegral . fromMaybe 0 . Map.lookup token
