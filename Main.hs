{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Main where


import           Control.Error
import           Options.Applicative
import           Prelude                    hiding (FilePath, mapM)

import           Opts
import           PopVox.Action.Transform
import           PopVox.Action.RankBills
import           PopVox.Action.ReportOn
import           PopVox.Action.SearchPosition
import           PopVox.Action.TestCsv
import           PopVox.Action.TestJson
import           PopVox.Types


main :: IO ()
main = execParser opts >>= popvox

popvox :: PopVoxOptions -> IO ()
popvox Transform{..} =
    runScript $ transform contribDataFile maplightAPIDir outputFile
popvox RankBills{..} =
    runScript $ rankBills rankBillScores rankBillBills rankBillIndex rankBillOutput
popvox TestJson{..}       = testJSON maplightAPIDir
popvox TestCsv{..}        = testCSV contribDataFile failFast
popvox SearchPosition{..} = runScript $ searchPosition maplightAPIDir maplightOrg
popvox ReportOn{..}       = runScript $ reportOn reportCsvFile reportTarget

