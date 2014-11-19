{-# LANGUAGE OverloadedStrings #-}


module PopVox.Types.Orgs
    ( OrgContribIndex
    , OrgContribIndex'
    , OrgContrib(..)
    , OrgInfo(..)
    ) where


import           Control.Applicative
import           Data.Aeson
import           Data.Csv             hiding ((.:))
import qualified Data.Csv             as CSV
import           Data.Monoid

import           PopVox.Types.Common
import           PopVox.Types.Contrib


type OrgContribIndex  = HashIndex OrgName ContribIndex
type OrgContribIndex' = HashIndex OrgName ContribIndex'

data OrgContrib = OrgContrib
                { orgContribName   :: !OrgName
                , orgContribEntry  :: !ContribEntry
                , orgContribAmount :: !Int
                } deriving (Show, Eq)

instance FromNamedRecord OrgContrib where
    parseNamedRecord m =   OrgContrib
                       <$> m CSV..: "DonorNameNormalized"   -- 48
                       <*> parseNamedRecord m
                       <*> m CSV..: "TransactionAmount"     -- 13

instance ToNamedRecord OrgContrib where
    toNamedRecord (OrgContrib n c a) =
        namedRecord [ "DonorNameNormalized" CSV..= toField n
                    , "TransactionAmount"   CSV..= toField a
                    ]
        <> toNamedRecord c


data OrgInfo  = OrgInfo !OrgName !Disposition
              deriving (Show)

instance FromJSON OrgInfo where
    parseJSON (Object o) =   OrgInfo
                         <$> o .: "name"
                         <*> o .: "disposition"
    parseJSON o          = fail $ "Invalid OrgInfo: " ++ show o

