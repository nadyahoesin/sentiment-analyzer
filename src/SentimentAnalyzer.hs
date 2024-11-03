module SentimentAnalyzer (sentimentAnalyzer, naiveBayesClassifier, preprocessTrainingData, evaluateAccuracy) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv ( decode, HasHeader(NoHeader) )
import qualified Data.Vector as V
import Data.Char (toLower, isAlpha)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (intersect)

type TextSentiment = (Int, String)
type WordFreqsByClass = (Map.Map String Int, Map.Map String Int)

sentimentAnalyzer :: IO ()
sentimentAnalyzer = do
    -- Membaca dataset
    csvData <- BL.readFile "updated_training.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> do
            let (testVector, trainVector) = V.splitAt (V.length v `div` 5) v

            let classFreqs = preprocessTrainingData trainVector

            -- putStrLn "Positive Token Frequencies:"
            -- print posFreq
            -- putStrLn "Negative Token Frequencies:"
            -- print negFreq

            putStr "Sentiment of \"Going on holiday this weekend\": "
            putStrLn $ if naiveBayesClassifier "Going on holiday this weekend" classFreqs == 4 then "positive" else "negative"

            putStr "Sentiment of \"Why is my code not working\": "
            putStrLn $ if naiveBayesClassifier "Why is my code not working" classFreqs == 4 then "positive" else "negative"

            putStr "Accuracy of model on testing data: "
            print $ evaluateAccuracy testVector classFreqs


{-----------------------------------------------------------------------------------------------------------------------------------------------------                               
                                                    PREPROCESSING FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------------------------------}

-- Mengubah 1 vektor berisi semua data menjadi tuple berisi map token ke jumlah kemunculan token untuk tiap kelas
preprocessTrainingData :: V.Vector TextSentiment -> WordFreqsByClass
preprocessTrainingData = removeUninformativeTokens . tMap (countTokensFreq . concatMap tokenize) . separateByClass

-- Mengubah 1 vektor berisi semua data menjadi tuple berisi 2 lists
-- list 1: semua teks dengan sentimen positif
-- list 2: semua teks dengan sentimen negatif
separateByClass :: V.Vector TextSentiment -> ([String], [String])
separateByClass = tMap (V.toList . V.map snd) . V.partition (\(sentiment, _) -> sentiment == 4)

-- Tokenisasi dan normalisasi teks menjadi token
tokenize :: String -> [String]
tokenize = words . map toLower . filter (\c -> isAlpha c || c == ' ')

-- Mengubah list of tokens menjadi map token ke jumlah kemunculan token
countTokensFreq :: [String] -> Map.Map String Int
countTokensFreq = foldr (\token -> Map.insertWith (+) token 1) Map.empty

-- Menghapus token yang sangat sering tetapi frequensi rata di tiap kelas, atau token yang sangat jarang
removeUninformativeTokens :: WordFreqsByClass -> WordFreqsByClass
removeUninformativeTokens freqs@(pos, neg) = tMap (Map.filter (> 5))
                                             (foldr Map.delete pos uninformativeTokens, foldr Map.delete neg uninformativeTokens)
    where uninformativeTokens = uncurry intersect $ tMap (Map.keys . Map.filter (> 450)) freqs

{-----------------------------------------------------------------------------------------------------------------------------------------------------                               
                                                    NAIVE BAYES FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------------------------------}

-- Naive Bayes classifier, memprediksi sentimen teks ketika diberi teks dan frekuensi kata tiap kelas
naiveBayesClassifier :: String -> WordFreqsByClass -> Int
naiveBayesClassifier text freqs
    | probPos >= probNeg = 4
    | otherwise = 0
    where
        (probPos, probNeg) = tZipWith (*) (computeClassProbs freqs) $ 
                             foldl1 (tZipWith (+)) [computeTokenGivenClassProbs token freqs | token <- tokenize text]
        tZipWith f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

-- Mendapatkan probabilitas tiap kelas
computeClassProbs :: WordFreqsByClass -> (Float, Float)
computeClassProbs (pos, neg) =  (getNumOfToken pos / numOfAllTokens, getNumOfToken neg / numOfAllTokens)
    where numOfAllTokens = getNumOfToken pos + getNumOfToken neg

-- Mendapatkan probabilitas untuk token given kelas untuk tiap kelas (dengan laplacian smoothing)
computeTokenGivenClassProbs :: String -> WordFreqsByClass -> (Float, Float)
computeTokenGivenClassProbs token (pos, neg) = ((countToken pos + 0.5) / (getNumOfToken pos + 1), (countToken neg + 0.5) / (getNumOfToken neg + 1))
    where countToken = fromIntegral . fromMaybe 0 . Map.lookup token

-- Mendapatkan jumlah token sebuah kelas
getNumOfToken :: Map.Map k Int -> Float
getNumOfToken = fromIntegral . Map.foldr (+) 0

-- Mengevaluasi akurasi Naive Bayes classifier terhadap testing data
evaluateAccuracy :: V.Vector TextSentiment -> WordFreqsByClass -> Float
evaluateAccuracy textSentiments freqs = numOfAccurate / fromIntegral (length textSentiments)
    where numOfAccurate = sum [1 | (sentiment, text) <- V.toList textSentiments, naiveBayesClassifier text freqs == sentiment]

-----------------------------------------------------------------------------------------------------------------------------------------------------                               

-- Map function over tuple with 2 elements
tMap :: (a -> b) -> (a, a) -> (b, b)
tMap f (x, y) = (f x, f y)