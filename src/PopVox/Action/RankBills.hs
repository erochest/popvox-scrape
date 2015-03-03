{-# LANGUAGE OverloadedStrings #-}


module PopVox.Action.RankBills
    ( rankBills
    ) where


import           Control.Applicative
import           Control.Error
import qualified Data.HashMap.Strict       as M
import           Data.Text.Format          hiding (print)
import qualified Data.Text.Format          as F
import           Data.Traversable
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (concat, decode)
import           Prelude                   hiding (FilePath, mapM)

import           PopVox.Bills
import           PopVox.Legislators
import           PopVox.Output
import           PopVox.Ranks


rankBills :: FilePath -> FilePath -> FilePath -> FilePath -> Script ()
rankBills rankBillScores rankBillBills rankBillIndex rankBillOutput = do
    scriptIO $ putStrLn "rank-bills"

    pscores <-  fmap (indexPositionScores . concat)
            .   mapM readPositionScores
            =<< scriptIO (listDirectory rankBillScores)
    F.print "Read {} pscores.\n" . Only $ M.size pscores

    lindex  <-  fmap M.unions
            .   mapM readLegislatorIndex
            =<< scriptIO (listDirectory rankBillIndex)
    F.print "Read {} legislator IDs from '{}'.\n" (M.size lindex, rankBillIndex)

    bills   <-  map (resolveIDs lindex) <$> readBillDataDir rankBillBills
    F.print "Read {} bill sponsor information.\n" . Only $ length bills

    scriptIO . writeBillRanks rankBillOutput $ assembleRanks pscores bills

