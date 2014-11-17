{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module PopVox.Output
    ( Header
    , writeOrgData
    , makeHeaderRow
    ) where


import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Csv
import           Data.Hashable
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as S
import qualified Data.List                  as L
import           Data.Monoid
import           Data.Ord
import           Data.Text.Encoding
import qualified Data.Vector                as V

import           PopVox.Types


makeHeaderRow :: OrgContribIndex -> OrgBillIndex -> Header
makeHeaderRow cindex bindex =
    V.fromList . map LB.toStrict $ L.concat [ ["Organization"]
                                            , sortHeaders (contribHeaders cindex)
                                            , sortHeaders (billHeaders bindex)
                                            , sortHeaders (totalHeaders cindex)
                                            ]

sortHeaders :: HeaderSet -> [LB.ByteString]
sortHeaders = L.sortBy (comparing (decodeUtf8 . LB.toStrict)) . S.toList

indexHeaders :: ColumnHead c => ([v] -> S.HashSet c) -> HashIndex k v -> HeaderSet
indexHeaders f = S.map (LB.fromStrict . encodeUtf8 . columnValue)
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
        undefined

toRow :: Header -> OrgData -> NamedRecord
toRow header (Org name contribs bills) =
        undefined
    {-
     - let rowMap = M.unions [ M.singleton "Organization" (encodeUtf8 name)
     -                       , indexContribs $ fmap getSum contribs
     -                       , indexBills bills
     -                       , partyTotals
     -                       ]
     - in  map (\k -> M.lookupDefault "" k rowMap) header
     - where
     -     partyTotals :: M.HashMap LB.ByteString LB.ByteString
     -     partyTotals = fmap (showbs . getSum)
     -                 . indexIndex (columnbs . contribParty)
     -                 $ unIndex contribs
     -}

columnbs :: ColumnHead c => c -> LB.ByteString
columnbs = LB.fromStrict . encodeUtf8 . columnValue

showbs :: Show s => s -> LB.ByteString
showbs = C8.pack . show

indexIndex :: (Eq k2, Hashable k2, Monoid v)
           => (k1 -> k2) -> M.HashMap k1 v -> M.HashMap k2 v
indexIndex f = M.fromListWith mappend . map (first f) . M.toList

indexContribs :: ContribIndex' -> M.HashMap LB.ByteString LB.ByteString
indexContribs = M.fromList . map (columnbs `bimap` showbs) . M.toList . unIndex

indexBills :: BillIndex -> M.HashMap LB.ByteString LB.ByteString
indexBills = M.fromList . map (columnbs `bimap` (showbs . fromEnum)) . M.toList

getKeySet :: (Hashable k, Eq k) => M.HashMap k v -> S.HashSet k
getKeySet = S.fromList . M.keys
