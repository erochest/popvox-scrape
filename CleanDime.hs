{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Main where


import           Control.Applicative
import qualified Data.ByteString.Lazy    as B
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding
import           Data.Traversable
import           Prelude                 hiding (map)

import           Debug.Trace


-- To parallelize:
-- * process the join-lines as part of a sequential first pass (although
--   this may kill any gains I get from parallelizing it, but maybe all those
--   `T.is*fixOf` will still cost a lot);
-- * Remove the fold/accum part of the central processing;
-- * Use https://hackage.haskell.org/package/parallel;
-- * Chunk and parallelize.

tracef :: Params ps => Format -> ps -> a -> a
tracef f ps = trace (T.unpack $ format f ps)


cleanDime :: T.Text -> T.Text
cleanDime = T.unlines
          . catMaybes
          . snd
          . mapAccumL cleanLine Nothing
          . T.lines

tags :: [T.Text]
tags = [ "NNE"
       , "PAC"
       , "CIT"
       , "CRV"
       , "NON"
       , "OTH"
       ]

replaceTag :: T.Text -> T.Text -> Maybe T.Text
replaceTag tag line
    | long  `T.isInfixOf` line = Just $ T.replace long  repl line
    | short `T.isInfixOf` line = Just $ T.replace short repl line
    | otherwise                = Nothing
    where
        long  = "\"" <> tag <> "\",\"\","
        short = "\"" <> tag <> "\","
        repl  = "\"\","

replaceTags :: T.Text -> Maybe T.Text
replaceTags line = foldl' step empty tags
    where
        step accum tag = accum <|> replaceTag tag line

invalidLine :: T.Text -> Maybe T.Text
invalidLine line
    | ("\"COMM\"" `T.isInfixOf` line) && not (isValidComm line)
        = Just $ recipComm line
    | otherwise = Nothing

isValidComm :: T.Text -> Bool
isValidComm line | "\"\",\"COMM\""    `T.isInfixOf` line = True
                 | "\"100\",\"COMM\"" `T.isInfixOf` line = True
                 | "\"200\",\"COMM\"" `T.isInfixOf` line = True
                 | "\"402\",\"COMM\"" `T.isInfixOf` line = True
                 | "\"0\",\"COMM\""   `T.isInfixOf` line = True
                 | otherwise                             = False

deleteLine :: (Maybe T.Text, Maybe T.Text)
deleteLine = (Nothing, Nothing)

cleanLine :: Maybe T.Text -> T.Text -> (Maybe T.Text, Maybe T.Text)
cleanLine prev line
    -- I just can't make sense of these lines.
    | "48d749ff199f19b3f8a9487d9648e33b" `T.isInfixOf` line = deleteLine

    | "33054178" `isInd2012` line = (Just line, Nothing)
    | "one\"\",\"\"none\"" `T.isPrefixOf` line
        = justPair $ maybe "" clean33054178a prev <> clean33054178b line

    | "30233408" `isInd2012` line = justPair $ clean30233408 line
    | "30233412" `isInd2012` line = justPair $ clean30233412 line
    | "\"100\",\"\",\"COMM\"" `T.isInfixOf` line = justPair $ recipComm100 line
    | "\"UNK\",\"COMM\""      `T.isInfixOf` line = justPair $ recipCommUNK line
    | otherwise = (Nothing, replaceTags line <|> invalidLine line <|> pure line)

justPair :: T.Text -> (Maybe T.Text, Maybe T.Text)
justPair = (Nothing,) . Just

isInd2012 :: T.Text -> T.Text -> Bool
isInd2012 n line = ("2012,\"e:ind:2012:" <> n <> "\",") `T.isPrefixOf` line

clean33054178a :: T.Text -> T.Text
clean33054178a = T.replace "\"ballwin\"" "\"\",\"ballwin\""
               . T.replace ",0,\"" ",\""
               . T.stripEnd

clean33054178b :: T.Text -> T.Text
clean33054178b = T.replace "\"142902012\"," ""
               . T.replace "\"CAND\"" "\"200\",\"CAND\""
               . federa
               . T.intercalate "\"" . removeN 2 "" . T.splitOn "\""

clean30233408 :: T.Text -> T.Text
clean30233408 = T.replace "\"I\",\"F\",\"\",\"35 fuller pl\","
                          "\"I\",\"F\",\"35 fuller pl\","
              . T.replace "\"iatse local 52\",\",\",\",\","
                          "\"iatse local 52\",\"\",\"\",\"\",\"\","

clean30233412 :: T.Text -> T.Text
clean30233412 = T.replace "\"I\",\"F\"" "\"I\",\"F\",\"\""
              . T.replace "\"NY\",\"12\",0," "\"NY\",\"12\","
              . T.replace "\"\",\"\"j" "\",\"j"
              . T.replace "\"C003700072012\"," ""
              . recipComm
              . federa

recipComm100 :: T.Text -> T.Text
recipComm100 = recipCommTag "100"

recipCommUNK :: T.Text -> T.Text
recipCommUNK = T.replace "\"UNK\",\"COMM\"" "\"\",\"COMM\""

recipCommTag :: T.Text -> T.Text -> T.Text
recipCommTag tag = T.replace (format "\"{}\",\"\",\"COMM\"" $ Only tag)
                             "\"\",\"COMM\""

recipCommTag' :: T.Text -> T.Text -> T.Text
recipCommTag' tag = T.replace (format "\"{}\",\"COMM\"" $ Only tag)
                              "\"\",\"COMM\""

federa :: T.Text -> T.Text
federa = T.replace "\"federa\",\"\"" "\"federa\""

recipComm :: T.Text -> T.Text
recipComm = T.replace ",\"COMM\"," ",\"\",\"COMM\","

removeN :: Eq a => Int -> a -> [a] -> [a]
removeN 0 _ xs = xs
removeN n y (x:xs) | y == x    = removeN (n - 1) y xs
                   | otherwise = x : removeN n y xs
removeN _ _ [] = []

main :: IO ()
main = B.interact (encodeUtf8 . cleanDime . decodeLatin1)
