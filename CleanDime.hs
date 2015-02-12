{-# LANGUAGE OverloadedStrings #-}


module Main where


import qualified Data.ByteString.Lazy    as B
import qualified Data.List               as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.IO       as TIO
import           Data.Traversable
import           Prelude                 hiding (map)

import Debug.Trace


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
    | "30233413" `isInd2012` line = justPair $ recipComm line
    | "30233414" `isInd2012` line = justPair $ recipComm line
    | "30233415" `isInd2012` line = justPair $ recipComm line

cleanLine _ (_, line) = justPair line

justPair :: T.Text -> (Maybe T.Text, Maybe T.Text)
justPair = (Nothing,) . Just

isInd2012 :: T.Text -> T.Text -> Bool
isInd2012 n line = "2012,\"e:ind:2012:" <> n <> "\"," `T.isPrefixOf` line

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
