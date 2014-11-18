{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module PopVox.Output
    ( Header
    , writeOrgData
    , makeHeaderRow
    ) where


import qualified Data.ByteString.Lazy as LB
import           Data.Csv
import           Data.Hashable
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
import qualified Data.List            as L
import           Data.Ord
import           Data.Text.Encoding
import qualified Data.Vector          as V

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
writeOrgData header out = LB.writeFile out
                        . encodeByName header
                        . map (WithHeader header)

getKeySet :: (Hashable k, Eq k) => M.HashMap k v -> S.HashSet k
getKeySet = S.fromList . M.keys
