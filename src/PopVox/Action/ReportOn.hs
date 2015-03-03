{-# LANGUAGE OverloadedStrings #-}


module PopVox.Action.ReportOn
    ( reportOn
    ) where


import           Control.Error
import           Data.Bifunctor
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as LB
import           Data.Csv                  hiding (Only, Parser)
import qualified Data.HashMap.Strict       as M
import qualified Data.Text                 as T
import           Data.Text.Format          hiding (print)
import qualified Data.Text.Format          as F
import qualified Data.Vector               as V
import           Filesystem.Path.CurrentOS hiding (concat, decode)
import           Prelude                   hiding (FilePath, mapM)


reportOn :: FilePath -> T.Text -> Script ()
reportOn reportCsvFile reportTarget =
        (EitherT . fmap decodeByName . LB.readFile . encodeString) reportCsvFile
    >>= V.mapM_ dumpOrg . V.filter isHit . snd
    where
        isHit = T.isInfixOf reportTarget . M.lookupDefault "" "Organization"

        dumpOrg :: M.HashMap B.ByteString T.Text -> Script ()
        dumpOrg org = scriptIO $ do
            mapM_ (F.print "\t{} => {}\n" . first Shown)
                . filter (not . T.null . snd)
                $ M.toList org
            putStrLn ""
