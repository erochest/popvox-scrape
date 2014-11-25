{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module PopVox.Types.Orgs
    ( OrgContribIndex
    , OrgContribIndex'
    , OrgContrib(..)

    , OrgInfo(..)
    , orgInfoName
    , orgInfoDisposition
    ) where


import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.Aeson           as A
import           Data.Csv             hiding ((.:))
import qualified Data.Csv             as CSV
import           Data.Monoid
import           GHC.Generics

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
              deriving (Show, Generic)

orgInfoName :: Lens' OrgInfo OrgName
orgInfoName f (OrgInfo n d) = fmap (`OrgInfo` d) (f n)

orgInfoDisposition :: Lens' OrgInfo Disposition
orgInfoDisposition f (OrgInfo n d) = fmap (OrgInfo n) (f d)

instance FromJSON OrgInfo where
    parseJSON (Object o) =   OrgInfo
                         <$> o .: "name"
                         <*> o .: "disposition"
    parseJSON o          = fail $ "Invalid OrgInfo: " ++ show o

instance ToJSON OrgInfo where
    toJSON (OrgInfo n d) = object [ "name"        A..= n
                                  , "disposition" A..= d
                                  ]
