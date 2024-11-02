module SentimentAnalyzer (main) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv ( decode, HasHeader(HasHeader) )
import qualified Data.Vector as V
import Data.Char (toLower, isAlpha)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (size)

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
            -- putStrLn "Positive Token Frequencies:"
            -- print posFreq
            -- putStrLn "Negative Token Frequencies:"
            -- print negFreq
            -- putStrLn "Neutral Token Frequencies:"
            -- print neuFreq

            -- putStrLn "Class Probabilities:"
            -- print $ computeClassProbs classFreqs

            -- putStrLn "Probability of token \"wonderful\" given each class:"
            -- print $ computeTokenGivenClassProbs "wonderful" classFreqs

            -- putStrLn "Probability of token \"terrible\" given each class:"
            -- print $ computeTokenGivenClassProbs "terrible" classFreqs

            -- putStrLn "Probability of token \"mediocre\" given each class:"
            -- print $ computeTokenGivenClassProbs "mediocre" classFreqs

            -- putStrLn "Sentiment of \"Happy happy happy but sad\""
            -- print $ naiveBayesClassifier "Happy happy happy but sad" classFreqs

            putStrLn "Accuracy of model on testing data:" 
            print $ evaluateAccuracy testVector classFreqs


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
computeClassProbs = getProbsAllClass False $ fromIntegral . Map.foldr (+) 0

-- Mendapatkan probabilitas untuk token given kelas untuk tiap kelas
computeTokenGivenClassProbs :: String -> WordFreqsByClass -> (Float, Float, Float)
computeTokenGivenClassProbs token = getProbsAllClass True $ fromIntegral . fromMaybe 0 . Map.lookup token

getProbsAllClass :: Bool -> (Map.Map String Int -> Float) -> WordFreqsByClass -> (Float, Float, Float)
getProbsAllClass smoothing f (pos, neg, neu)
    | smoothing = ((f pos + 1) / (x + numOfUniqueTokens), f neg + 1 / (x + numOfUniqueTokens), f neu + 1 / (x + numOfUniqueTokens))
    | otherwise = (f pos / x, f neg / x, f neu / x)
    where
        x = f pos + f neg + f neu
        numOfUniqueTokens = fromIntegral $ size (Map.keysSet pos) + size (Map.keysSet neg) + size (Map.keysSet neu)

-- Naive Bayes classifier, diberi text dan memprediksi sentimen text tersebut
naiveBayesClassifier :: String -> WordFreqsByClass -> String
naiveBayesClassifier text freqs
    | probPos > probNeg && probPos > probNeu = "positive"
    | probNeg > probNeu = "negative"
    | otherwise = "neutral"
    where
        (probPos, probNeg, probNeu) = foldl1 (t3ZipWith (+))
                                      [t3ZipWith (*) (computeTokenGivenClassProbs token freqs) classProbs | token <- tokenize text]
        classProbs = computeClassProbs freqs
        t3ZipWith f (x1, y1, z1) (x2, y2, z2) = (f x1 x2, f y1 y2, f z1 z2)

evaluateAccuracy :: V.Vector TextSentiment -> WordFreqsByClass -> Float
evaluateAccuracy textSentiments freqs = numOfAccurate / fromIntegral (length textSentiments)
    where numOfAccurate = sum [1 | (text, sentiment) <- V.toList textSentiments, naiveBayesClassifier text freqs == sentiment]