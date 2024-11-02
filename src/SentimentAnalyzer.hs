module SentimentAnalyzer (main) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv ( decode, HasHeader(HasHeader, NoHeader) )
import qualified Data.Vector as V
import Data.Char (toLower, isAlpha)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (size)
import Data.List (intersect, union)

type TextSentiment = (Int, String)
type WordFreqsByClass = (Map.Map String Int, Map.Map String Int)

main :: IO ()
main = do
    -- Membaca dataset
    csvData <- BL.readFile "updated_training.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> do
            let (testVector, trainVector) = V.splitAt (V.length v `div` 5) v
            print $ length $ fst $ preprocessTrainingData trainVector
            print $ length $ fst $ removeUninformativeTokens $ preprocessTrainingData trainVector

            let classFreqs@(posFreq, negFreq) = removeUninformativeTokens $ preprocessTrainingData trainVector

            putStrLn "Positive Token Frequencies:"
            print posFreq
            putStrLn "Negative Token Frequencies:"
            print negFreq

            putStrLn "Sentiment of \"Happy happy happy but sad\":"
            putStrLn $ if naiveBayesClassifier "Happy happy happy but sad" classFreqs == 4 then "positive" else "negative"

            putStrLn "Accuracy of model on testing data:"
            print $ evaluateAccuracy testVector classFreqs


-- Mengubah 1 vektor berisi semua data menjadi tuple berisi map token ke jumlah kemunculan token untuk tiap kelas
preprocessTrainingData :: V.Vector TextSentiment -> WordFreqsByClass
preprocessTrainingData = tMap (countTokensFreq . concatMap tokenize) . separateByClass

-- Mengubah 1 vektor berisi semua data menjadi tuple berisi 3 lists
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

-- Menghapus token yang sering tetapi frequensi rata di tiap kelas
removeUninformativeTokens :: WordFreqsByClass -> WordFreqsByClass
removeUninformativeTokens freqs@(pos, neg) = tMap (Map.filter (> 25)) 
                                             (foldr Map.delete pos uninformativeTokens, foldr Map.delete neg uninformativeTokens)
    where
        uninformativeTokens = topPos `intersect` topNeg
        (topPos, topNeg) = tMap (Map.keys . Map.filter (> 250)) freqs

-- Mendapatkan probabilitas tiap kelas
computeClassProbs :: WordFreqsByClass -> (Float, Float)
computeClassProbs (pos, neg) =  (getNumOfToken pos / numOfAllTokens, getNumOfToken neg / numOfAllTokens)
    where numOfAllTokens = getNumOfToken pos + getNumOfToken neg

-- Mendapatkan probabilitas untuk token given kelas untuk tiap kelas
computeTokenGivenClassProbs :: String -> WordFreqsByClass -> (Float, Float)
computeTokenGivenClassProbs token (pos, neg) = ((countToken pos + 0.5) / (getNumOfToken pos + 1), (countToken neg + 0.5) / (getNumOfToken neg + 1))
    where countToken = fromIntegral . fromMaybe 0 . Map.lookup token
        
getNumOfToken :: Map.Map k Int -> Float
getNumOfToken = fromIntegral . Map.foldr (+) 0

-- Naive Bayes classifier, diberi text dan memprediksi sentimen text tersebut
naiveBayesClassifier :: String -> WordFreqsByClass -> Int
naiveBayesClassifier text freqs
    | probPos >= probNeg = 4
    | otherwise = 0
    where
        (probPos, probNeg) = foldl1 (tZipWith (+))
                             [tZipWith (*) (computeTokenGivenClassProbs token freqs) classProbs | token <- tokenize text]
        classProbs = computeClassProbs freqs
        tZipWith f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

-- Mengevaluasi akurasi Naive Bayes classifier terhadap testing data
evaluateAccuracy :: V.Vector TextSentiment -> WordFreqsByClass -> Float
evaluateAccuracy textSentiments freqs = numOfAccurate / fromIntegral (length textSentiments)
    where numOfAccurate = sum [1 | (sentiment, text) <- V.toList textSentiments, naiveBayesClassifier text freqs == sentiment]

-- Map function over tuple with 2 elements
tMap :: (a -> b) -> (a, a) -> (b, b)
tMap f (x, y) = (f x, f y)