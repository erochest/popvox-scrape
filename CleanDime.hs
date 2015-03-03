{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Main where


import           Control.Applicative
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy        as B
import           Data.Foldable
import           Data.List.Split             (chunksOf)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                   as T
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Data.Text.Lazy              as TL
import           Data.Text.Lazy.Encoding
import           Data.Traversable
import           Prelude                     hiding (concat, map)

import           Debug.Trace


chunkSize :: Int
chunkSize = 4096

tracef :: Params ps => Format -> ps -> a -> a
tracef f ps = trace (TL.unpack $ format f ps)


cleanDime :: [TL.Text] -> [TL.Text]
cleanDime = concat
          . parMap rdeepseq (fmap TL.fromStrict . mapMaybe (cleanLine . TL.toStrict))
          . chunksOf chunkSize
          . catMaybes
          . snd
          . mapAccumL joinLines Nothing

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

joinLines :: Maybe TL.Text -> TL.Text -> (Maybe TL.Text, Maybe TL.Text)
joinLines prev line
    | "33054178" `isInd2012` line = (Just line, Nothing)
    | "one\"\",\"\"none\"" `TL.isPrefixOf` line
        = justPair $ maybe "" clean33054178a prev <> clean33054178b line
    | otherwise = (prev, Just line)

cleanLine :: T.Text -> Maybe T.Text
cleanLine line
    -- I just can't make sense of these lines.
    | "48d749ff199f19b3f8a9487d9648e33b" `T.isInfixOf` line = Nothing

    | "30233408" `isInd2012'` line = Just $ clean30233408 line
    | "30233412" `isInd2012'` line = Just $ clean30233412 line
    | "\"100\",\"\",\"COMM\"" `T.isInfixOf` line = Just $ recipComm100 line
    | "\"UNK\",\"COMM\""      `T.isInfixOf` line = Just $ recipCommUNK line
    | otherwise = replaceTags line <|> invalidLine line <|> pure line

justPair :: a -> (Maybe a, Maybe a)
justPair = (Nothing,) . Just

isInd2012 :: TL.Text -> TL.Text -> Bool
isInd2012 n line = ("2012,\"e:ind:2012:" <> n <> "\",") `TL.isPrefixOf` line

isInd2012' :: T.Text -> T.Text -> Bool
isInd2012' n line = ("2012,\"e:ind:2012:" <> n <> "\",") `T.isPrefixOf` line

clean33054178a :: TL.Text -> TL.Text
clean33054178a = TL.replace "\"ballwin\"" "\"\",\"ballwin\""
               . TL.replace ",0,\"" ",\""
               . TL.stripEnd

clean33054178b :: TL.Text -> TL.Text
clean33054178b = TL.replace "\"142902012\"," ""
               . TL.replace "\"CAND\"" "\"200\",\"CAND\""
               . federa
               . TL.intercalate "\"" . removeN 2 "" . TL.splitOn "\""

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
              . federa'

recipComm100 :: T.Text -> T.Text
recipComm100 = recipCommTag "100"

recipCommUNK :: T.Text -> T.Text
recipCommUNK = T.replace "\"UNK\",\"COMM\"" "\"\",\"COMM\""

recipCommTag :: T.Text -> T.Text -> T.Text
recipCommTag tag = T.replace ( TL.toStrict . format "\"{}\",\"\",\"COMM\""
                             $ Only tag
                             )
                              "\"\",\"COMM\""

recipCommTag' :: T.Text -> T.Text -> T.Text
recipCommTag' tag = T.replace (TL.toStrict . format "\"{}\",\"COMM\"" $ Only tag)
                               "\"\",\"COMM\""

federa :: TL.Text -> TL.Text
federa = TL.replace "\"federa\",\"\"" "\"federa\""

federa' :: T.Text -> T.Text
federa' = T.replace "\"federa\",\"\"" "\"federa\""

recipComm :: T.Text -> T.Text
recipComm = T.replace ",\"COMM\"," ",\"\",\"COMM\","

removeN :: Eq a => Int -> a -> [a] -> [a]
removeN 0 _ xs = xs
removeN n y (x:xs) | y == x    = removeN (n - 1) y xs
                   | otherwise = x : removeN n y xs
removeN _ _ [] = []

main :: IO ()
main = B.interact (encodeUtf8 . TL.unlines . cleanDime . TL.lines . decodeLatin1)
