{-# LANGUAGE OverloadedStrings #-}


module PopVox.Action.TestCsv
    ( testCSV
    ) where


import           Control.Applicative
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Csv.Incremental       hiding (Parser, decodeByName)
import qualified Data.Csv.Incremental       as Csv
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Format           hiding (print)
import qualified Data.Text.Format           as F
import           Data.Traversable
import           Filesystem.Path.CurrentOS
import           Prelude                    hiding (FilePath, mapM)
import           System.IO                  (hFlush, stdout)

import           PopVox.Types               hiding (contribDataFile, failFast)


testCSV :: FilePath -> Bool -> IO ()
testCSV contribDataFile failFast = do
    let fn = encodeString contribDataFile
    F.print "Reading {}... " $ Only fn
    hFlush stdout
    s <- map (<> "\n") . L8.lines <$> L8.readFile fn
    let header' = Csv.decodeByName :: HeaderParser (Csv.Parser OrgContrib)
        (headerOut, dataLines) = parseHeader header' s
    case headerOut of
        FailH dataLine er -> do
            F.print "HEADER ERROR: {}\n" $ Only er
            C8.putStrLn dataLine
        PartialH _ -> putStrLn "Oops! PartialH!"
        DoneH _ parser ->
            mapM_ print $ parseData parser dataLines
    putStrLn ""

    where
        parseHeader f@(FailH _ _) xs    = (f, xs)
        parseHeader (PartialH f) (x:xs) = parseHeader (f $ LB.toStrict x) xs
        parseHeader (PartialH f) []     = parseHeader (f B.empty) []
        parseHeader d@(DoneH _ _) xs    = (d, xs)

        parseData p = go p 2

        go (Fail _ er)   n _      =  [(n, er)]
        go (Done outs)   n _      =  leftLines n outs
        go (Many outs f) n []     =  leftLines n outs
                                  ++ go (f B.empty) (n + length outs) []
        go (Many outs f) n (x:xs) =
            let rest = go (f $ LB.toStrict x) (n + length outs) xs
            in  case leftLines n outs of
                    []  -> rest
                    xs' | failFast  -> xs'
                        | otherwise -> xs' ++ rest

        lft (Left x)  = Just x
        lft (Right _) = Nothing

        leftLines n = mapMaybe (sequenceA . fmap lft) . zip [n..]

