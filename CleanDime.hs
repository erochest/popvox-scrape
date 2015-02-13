{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Main where


import qualified Data.ByteString.Lazy    as B
import qualified Data.List               as L
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Format
import qualified Data.Text.Format        as F
import           Data.Text.Format.Params
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.IO       as TIO
import           Data.Traversable
import           Prelude                 hiding (map)

import           Debug.Trace


tracef :: Params ps => Format -> ps -> a -> a
tracef f ps = trace (T.unpack $ format f ps)


cleanDime :: T.Text -> T.Text
cleanDime = T.unlines
          . catMaybes
          . snd
          . mapAccumL cleanLine Nothing
          . zip [1::Int ..]
          . T.lines

cleanLine :: Maybe T.Text -> (Int, T.Text) -> (Maybe T.Text, Maybe T.Text)

cleanLine _           (269809, line) = (Just line, Nothing)
cleanLine (Just prev) (269810, line) = justPair $  clean33054178a prev
                                                <> clean33054178b line

cleanLine _ (_, line)
    | "30233408" `isInd2012` line = justPair $ clean30233408 line
    | "30233412" `isInd2012` line = justPair $ clean30233412 line

    | "\"100\",\"\",\"COMM\"" `T.isInfixOf` line = justPair $ recipComm100 line
    | "\"UNK\",\"COMM\""      `T.isInfixOf` line = justPair $ recipCommUNK line
    | "\"NNE\",\"\",\"COMM\"" `T.isInfixOf` line = justPair $ recipCommNNE line
    | "\"NNE\",\"COMM\""      `T.isInfixOf` line = justPair $ recipCommNNE' line
    | "\"PAC\",\"\",\"COMM\"" `T.isInfixOf` line = justPair $ recipCommPAC line
    | "\"PAC\",\"COMM\""      `T.isInfixOf` line = justPair $ recipCommPAC' line
    | "\"CIT\",\"\",\"COMM\"" `T.isInfixOf` line = justPair $ recipCommCIT line
    | "\"CIT\",\"COMM\""      `T.isInfixOf` line = justPair $ recipCommCIT' line
    | "\"CRV\",\"\",\"COMM\"" `T.isInfixOf` line = justPair $ recipCommCRV line
    | "\"CRV\",\"COMM\""      `T.isInfixOf` line = justPair $ recipCommCRV' line
    | "\"NON\",\"\",\"COMM\"" `T.isInfixOf` line = justPair $ recipCommNON line
    | "\"NON\",\"COMM\""      `T.isInfixOf` line = justPair $ recipCommNON' line
    | "\"OTH\",\"\",\"COMM\"" `T.isInfixOf` line = justPair $ recipCommOTH line
    | "\"OTH\",\"COMM\""      `T.isInfixOf` line = justPair $ recipCommOTH' line

    | ("\"COMM\"" `T.isInfixOf` line) && not (isValidComm line)
        = justPair $ recipComm line

cleanLine _ (_, line) = justPair line

isValidComm :: T.Text -> Bool
isValidComm line | "\"\",\"COMM\""    `T.isInfixOf` line = True
                 | "\"100\",\"COMM\"" `T.isInfixOf` line = True
                 | "\"200\",\"COMM\"" `T.isInfixOf` line = True
                 | "\"402\",\"COMM\"" `T.isInfixOf` line = True
                 | "\"0\",\"COMM\""   `T.isInfixOf` line = True
                 | otherwise                             = False

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

recipCommNNE :: T.Text -> T.Text
recipCommNNE = recipCommTag "NNE"

recipCommNNE' :: T.Text -> T.Text
recipCommNNE' = recipCommTag' "NNE"

recipCommPAC :: T.Text -> T.Text
recipCommPAC = recipCommTag "PAC"

recipCommPAC' :: T.Text -> T.Text
recipCommPAC' = recipCommTag' "PAC"

recipCommCIT :: T.Text -> T.Text
recipCommCIT = recipCommTag "CIT"

recipCommCIT' :: T.Text -> T.Text
recipCommCIT' = recipCommTag' "CIT"

recipCommCRV :: T.Text -> T.Text
recipCommCRV = recipCommTag "CRV"

recipCommCRV' :: T.Text -> T.Text
recipCommCRV' = recipCommTag' "CRV"

recipCommNON :: T.Text -> T.Text
recipCommNON = recipCommTag "NON"

recipCommNON' :: T.Text -> T.Text
recipCommNON' = recipCommTag' "NON"

recipCommOTH :: T.Text -> T.Text
recipCommOTH = recipCommTag "OTH"

recipCommOTH' :: T.Text -> T.Text
recipCommOTH' = recipCommTag' "OTH"

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

main :: IO ()
main = B.interact (encodeUtf8 . cleanDime . decodeLatin1)
