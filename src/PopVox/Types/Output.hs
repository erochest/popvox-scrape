{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module PopVox.Types.Output
    ( OrgBillIndex
    , OrgData(..)
    , BillRankData(..)
    ) where


import           Data.Bifunctor        (bimap, first)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Csv              hiding ((.:))
import qualified Data.Csv              as CSV
import           Data.Hashable
import qualified Data.HashMap.Strict   as M
import           Data.Monoid
import           Data.Text.Encoding    (encodeUtf8)
import           GHC.Generics

import           PopVox.Types.Bills
import           PopVox.Types.Common
import           PopVox.Types.Contrib


type OrgBillIndex     = HashIndex OrgName BillIndex

data OrgData = Org
             { orgName     :: !OrgName
             , orgContribs :: !ContribIndex
             , orgBills    :: !BillIndex
             }

instance ToNamedRecord OrgData where
    toNamedRecord (Org n conts bills) =
        namedRecord $ concat [ ["Organization" CSV..= n]
                             , contribsRecord $ fmap getSum conts
                             , billsRecord bills
                             , totalsRecord conts
                             ]


contribsRecord :: ContribIndex' -> [(BS.ByteString, BS.ByteString)]
contribsRecord = map (columnbs `bimap` showbs) . M.toList . unIndex

billsRecord :: BillIndex -> [(BS.ByteString, BS.ByteString)]
billsRecord = map (columnbs `bimap` (showbs . fromEnum)) . M.toList . unIndex

totalsRecord :: ContribIndex -> [(BS.ByteString, BS.ByteString)]
totalsRecord = M.toList
             . fmap (showbs . getSum)
             . indexIndex (columnbs . contribParty)
             . unIndex

columnbs :: ColumnHead c => c -> BS.ByteString
columnbs = encodeUtf8 . columnValue

showbs :: Show s => s -> BS.ByteString
showbs = C8.pack . show

indexIndex :: (Eq k2, Hashable k2, Monoid v)
           => (k1 -> k2) -> M.HashMap k1 v -> M.HashMap k2 v
indexIndex f = M.fromListWith mappend . map (first f) . M.toList


data BillRankData = BillRankData
                  { billRankDataBill     :: !Bill
                  , billRankSponsorCount :: !Int
                  , billRankDemSupport   :: !Int
                  , billRankGOPSupport   :: !Int
                  , billRankDataPosition :: !Score
                  } deriving (Show, Eq, Generic)

instance ToNamedRecord BillRankData where
    toNamedRecord (BillRankData bill count dem gop score) =
        namedRecord [ "Bill"          CSV..= bill
                    , "Congress"      CSV..= getSession (billSession bill)
                    , "Sponsor Count" CSV..= count
                    , "Dem Support"   CSV..= dem
                    , "GOP Support"   CSV..= gop
                    , "Score"         CSV..= score
                    ]
