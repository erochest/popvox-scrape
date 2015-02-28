{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module PopVox.Types.Orgs
    ( OrgContribIndex
    , OrgContribIndex'
    , OrgContrib(..)

    , OrgInfo(..)
    , orgInfoID
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
import qualified Data.Text            as T
import           GHC.Generics

import           PopVox.Types.Common
import           PopVox.Types.Contrib


type OrgContribIndex  = HashIndex OrgName ContribIndex
type OrgContribIndex' = HashIndex OrgName ContribIndex'

data OrgContrib = OrgContrib
                { orgContribName   :: !OrgName
                , orgDistrict10s   :: !T.Text
                , orgContribEntry  :: !ContribEntry
                , orgContribAmount :: !Amount
                } deriving (Show, Eq)

instance FromNamedRecord OrgContrib where
    parseNamedRecord m =   OrgContrib
                       <$> m CSV..: "contributor_name"
                       <*> m CSV..: "contributor_district_10s"
                       <*> parseNamedRecord m
                       <*> m CSV..: "amount"

instance ToNamedRecord OrgContrib where
    toNamedRecord (OrgContrib n d c a) =
        namedRecord [ "contributor_name"         CSV..= toField n
                    , "contributor_district_10s" CSV..= toField d
                    , "amount"                   CSV..= toField a
                    ]
        <> toNamedRecord c


data OrgInfo  = OrgInfo !OrgID !OrgName !Disposition
              deriving (Show, Generic)

orgInfoID :: Lens' OrgInfo OrgID
orgInfoID f (OrgInfo i n d) = fmap (\i' -> OrgInfo i' n d) (f i)

orgInfoName :: Lens' OrgInfo OrgName
orgInfoName f (OrgInfo i n d) = fmap (\n' -> OrgInfo i n' d) (f n)

orgInfoDisposition :: Lens' OrgInfo Disposition
orgInfoDisposition f (OrgInfo i n d) = fmap (OrgInfo i n) (f d)

instance FromJSON OrgInfo where
    parseJSON (Object o) =   OrgInfo
                         <$> o .: "organization_id"
                         <*> o .: "name"
                         <*> o .: "disposition"
    parseJSON o          = fail $ "Invalid OrgInfo: " ++ show o

instance ToJSON OrgInfo where
    toJSON (OrgInfo i n d) = object [ "organization_id" A..= i
                                    , "name"            A..= n
                                    , "disposition"     A..= d
                                    ]
