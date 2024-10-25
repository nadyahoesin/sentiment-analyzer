module SentimentAnalyzer (main) where

-- import qualified Data.ByteString.Lazy as BL
-- import Data.Csv ( decode, HasHeader(HasHeader) )
import qualified Data.Vector as V
import Data.Char (toLower, isAlpha)
import qualified Data.Map as Map

type TextSentiment = (String, String)

main :: IO ()
main = do
    -- csvData <- BL.readFile "chat_dataset.csv"
    -- case decode HasHeader csvData of
    --     Left err -> putStrLn err
    --     Right v -> do
            -- let (testVector, train) = V.splitAt (div (V.length v) 3) v
    let tokens = tokenize "hal*&(o saya i'm nadya89828 nadya"
    print tokens
    print $ countTokensFreq tokens

-- Mengubah 1 vektor berisi semua data menjadi tuple berisi map token ke jumlah kemunculan token untuk semua kelas
preprocessTrainingData :: V.Vector TextSentiment -> (Map.Map String Int, Map.Map String Int, Map.Map String Int)
preprocessTrainingData v = (preprocess pos, preprocess neg, preprocess neu)
    where
        (pos, neg, neu) = separateByClass v
        preprocess = countTokensFreq . concatMap tokenize

-- Mengubah 1 vektor berisi semua data menjadi tuple berisi 3 lists
-- list 1: semua text dengan sentimen positif
-- list 2: semua text dengan sentimen negatif
-- list 3: semua text dengan sentimen netral
separateByClass :: V.Vector TextSentiment -> ([String], [String], [String])
separateByClass v = (positive, negative, neutral)
    where
        positive = getTexts $ V.filter (\(_,sentiment) -> sentiment == "positive") v
        negative = getTexts $ V.filter (\(_,sentiment) -> sentiment == "negative") v
        neutral = getTexts $ V.filter (\(_,sentiment) -> sentiment == "neutral") v
        getTexts = V.toList . V.map fst

-- Tokenisasi dan normalisasi teks menjadi token
tokenize :: String -> [String]
tokenize = words . map toLower . filter (\c -> isAlpha c || c == ' ')

-- Mengubah list of tokens menjadi map token ke jumlah kemunculan token
countTokensFreq :: [String] -> Map.Map String Int
countTokensFreq = foldr (\token acc -> Map.insertWith (+) token 1 acc) Map.empty