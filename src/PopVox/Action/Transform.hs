{-# LANGUAGE OverloadedStrings #-}


module PopVox.Action.Transform
    ( transform
    ) where


import           Control.Applicative
import           Control.Error
import           Data.Text.Format          hiding (print)
import qualified Data.Text.Format          as F
import           Data.Traversable
import           Filesystem.Path.CurrentOS hiding (concat, decode)
import           Prelude                   hiding (FilePath, mapM)

import           PopVox.Contribs
import           PopVox.MapLight
import           PopVox.Output
import           PopVox.Types              hiding (contribDataFile,
                                            maplightAPIDir, outputFile)
import           PopVox.Utils


transform :: FilePath -> FilePath -> FilePath -> Script ()
transform contribDataFile maplightAPIDir outputFile = do
    scriptIO $ putStrLn "\nQuerying maplight.org...\n"
    bIndex  <-  indexBills . concat
            <$> mapM (billList maplightAPIDir
                        `withLog` "\tQuerying for session {}...\n"
                     ) sessions
    scriptIO $ do
        dumpBillIndex bIndex
        F.print "\nReading contributor data from {}...\n"
            . Only $ encodeString contribDataFile
    ocIndex <- readIndexContribs contribDataFile
    scriptIO $ do
        dumpContribIndex ocIndex
        F.print "\nWriting data to {}...\n" $ Only outputFile
        writeOrgData (makeHeaderRow ocIndex bIndex)
                     outputFile
                     (toData bIndex ocIndex)

        putStrLn "\ndone\n"

