{-# LANGUAGE OverloadedStrings #-}


module PopVox.Output
    ( Header
    , writeOrgData
    ) where


import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List     as CL
import           Data.CSV.Conduit
import           Data.Hashable
import qualified Data.HashMap.Strict   as M
import           Data.Monoid
import           Data.Text.Encoding

import           PopVox.Types


writeOrgData :: Header -> FilePath -> [OrgData] -> IO ()
writeOrgData header out xs =
    runResourceT $  CL.sourceList xs
                 $= CL.map (toRow header)
                 $= prepend header
                 $= fromCSV defCSVSettings
                 $$ sinkFile out

prepend :: Monad m => a -> Conduit a m a
prepend x = yield x >> go
    where go = maybe (return ()) (const go <=< yield) =<< await

toRow :: Header -> OrgData -> Row BS.ByteString
toRow header (Org name contribs bills) =
    let rowMap = M.unions [ M.singleton "Organization" (encodeUtf8 name)
                          , indexMapRow contribs
                          , indexMapRow' bills
                          , partyTotals
                          ]
    in  map (\k -> M.lookupDefault "" k rowMap) header
    where
        partyTotals :: M.HashMap BS.ByteString BS.ByteString
        partyTotals = fmap (showbs . getSum)
                    . indexIndex (columnbs . contribParty)
                    . fmap Sum
                    $ unIndex contribs

columnbs :: ColumnHead c => c -> BS.ByteString
columnbs = encodeUtf8 . columnValue

showbs :: Show s => s -> BS.ByteString
showbs = C8.pack . show

indexIndex :: (Eq k2, Hashable k2, Monoid v)
           => (k1 -> k2) -> M.HashMap k1 v -> M.HashMap k2 v
indexIndex f = M.fromListWith mappend . map (first f) . M.toList

indexMapRow :: (ColumnHead k, Show v)
            => HashIndex k v -> M.HashMap BS.ByteString BS.ByteString
indexMapRow = M.fromList . map (columnbs `bimap` showbs) . M.toList . unIndex

indexMapRow' :: (ColumnHead k, Show v)
             => M.HashMap k v -> M.HashMap BS.ByteString BS.ByteString
indexMapRow' = M.fromList . map (columnbs `bimap` showbs) . M.toList
