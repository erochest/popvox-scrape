{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module PopVox.Output
    ( Header
    , writeOrgData
    , makeHeaderRow
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
import qualified Data.HashSet          as S
import qualified Data.List             as L
import           Data.Monoid
import           Data.Ord
import           Data.Text.Encoding

import           PopVox.Types


makeHeaderRow :: OrgContribIndex -> OrgBillIndex -> Header
makeHeaderRow cindex bindex = L.concat [ ["Organization"]
                                         , sortHeaders (contribHeaders cindex)
                                         , sortHeaders (billHeaders bindex)
                                         , sortHeaders (totalHeaders cindex)
                                         ]

sortHeaders :: HeaderSet -> Header
sortHeaders = L.sortBy (comparing decodeUtf8) . S.toList

indexHeaders :: ColumnHead c => ([v] -> S.HashSet c) -> HashIndex k v -> HeaderSet
indexHeaders f = S.map (encodeUtf8 . columnValue)
               . f
               . map snd . M.toList . unIndex

contribHeaders :: OrgContribIndex -> HeaderSet
contribHeaders = indexHeaders (S.unions . map (getKeySet . unIndex))

billHeaders :: OrgBillIndex -> HeaderSet
billHeaders = indexHeaders (S.fromList . map fst . concatMap M.toList)

totalHeaders :: OrgContribIndex -> HeaderSet
totalHeaders = indexHeaders ( S.fromList
                            . map contribParty
                            . L.concatMap (M.keys . unIndex)
                            )

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
                          , indexContribs $ fmap getSum contribs
                          , indexBills bills
                          , partyTotals
                          ]
    in  map (\k -> M.lookupDefault "" k rowMap) header
    where
        partyTotals :: M.HashMap BS.ByteString BS.ByteString
        partyTotals = fmap (showbs . getSum)
                    . indexIndex (columnbs . contribParty)
                    $ unIndex contribs

columnbs :: ColumnHead c => c -> BS.ByteString
columnbs = encodeUtf8 . columnValue

showbs :: Show s => s -> BS.ByteString
showbs = C8.pack . show

indexIndex :: (Eq k2, Hashable k2, Monoid v)
           => (k1 -> k2) -> M.HashMap k1 v -> M.HashMap k2 v
indexIndex f = M.fromListWith mappend . map (first f) . M.toList

indexContribs :: ContribIndex' -> M.HashMap BS.ByteString BS.ByteString
indexContribs = M.fromList . map (columnbs `bimap` showbs) . M.toList . unIndex

indexBills :: BillIndex -> M.HashMap BS.ByteString BS.ByteString
indexBills = M.fromList . map (columnbs `bimap` (showbs . fromEnum)) . M.toList

getKeySet :: (Hashable k, Eq k) => M.HashMap k v -> S.HashSet k
getKeySet = S.fromList . M.keys
