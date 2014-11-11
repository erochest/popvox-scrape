{-# LANGUAGE OverloadedStrings #-}


module PopVox.Output
    ( writeOrgData
    ) where


import           Data.Bifunctor
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List     as CL
import           Data.CSV.Conduit
import qualified Data.HashMap.Strict   as HM
import qualified Data.Map.Strict       as M
import           Data.Monoid
import           Data.Text.Encoding

import           PopVox.Types


writeOrgData :: FilePath -> [OrgData] -> IO ()
writeOrgData out xs =
    runResourceT $  CL.sourceList xs
                 $= CL.map toMapRow
                 $= (writeHeaders defCSVSettings >> fromCSV defCSVSettings)
                 $$ sinkFile out

toMapRow :: OrgData -> MapRow BS.ByteString
toMapRow (Org name contribs bills) =
    M.unions [ M.singleton "Organization" (encodeUtf8 name)
             , indexMapRow contribs
             , indexMapRow bills
             , partyTotals
             ]
    where
        partyTotals :: MapRow BS.ByteString
        partyTotals = fmap (showbs . getSum)
                    . indexIndex (columnbs . contribParty)
                    . fmap Sum
                    $ unIndex contribs

columnbs :: ColumnHead c => c -> BS.ByteString
columnbs    = encodeUtf8 . columnValue

showbs :: Show s => s -> BS.ByteString
showbs      = C8.pack . show

indexIndex :: (Ord k2, Monoid v) => (k1 -> k2) -> HM.HashMap k1 v -> M.Map k2 v
indexIndex f = M.fromListWith mappend . map (first f) . HM.toList

indexMapRow :: (ColumnHead k, Show v) => HashIndex k v -> MapRow BS.ByteString
indexMapRow = M.fromList . map (columnbs `bimap` showbs) . HM.toList . unIndex
