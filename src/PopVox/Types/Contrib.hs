{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module PopVox.Types.Contrib
    ( ContribType(..)
    , ContribEntry(..)
    , ContribIndex
    , ContribIndex'
    ) where


import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types       (defaultOptions)
import           Data.Csv               hiding ((.:))
import qualified Data.Csv               as CSV
import           Data.Hashable
import           Data.Monoid
import qualified Data.Text.Lazy.Builder as B
import           GHC.Generics

import           PopVox.Types.Common


type ContribIndex  = HashIndex ContribEntry (Sum Int)
type ContribIndex' = HashIndex ContribEntry Int

data ContribType = Contribution
                 | Expenditure
                 deriving (Show, Eq, Enum, Ord, Bounded, Generic)

instance Hashable ContribType

instance FromField ContribType where
    parseField "Independent Expenditor (Person or Group)" = pure Expenditure
    parseField "Single Candidate Independent Expenditure" = pure Expenditure
    parseField ""                                         = pure Contribution
    parseField _                                          = pure Contribution

instance ToField ContribType where
    toField Expenditure  = "Single Candidate Independent Expenditure"
    toField Contribution = "Contribution"

instance ColumnHead ContribType where
    columnValue   Expenditure  = "Ind"
    columnValue   Contribution = ""
    columnBuilder Expenditure  = "Ind"
    columnBuilder Contribution = ""

instance ToJSON ContribType where
    toJSON = genericToJSON defaultOptions


data ContribEntry = Contrib
                  { contribParty :: !Party
                  , contribType  :: !ContribType
                  , contribYear  :: !Year
                  } deriving (Show, Eq, Ord, Generic)

instance Hashable ContribEntry

instance ColumnHead ContribEntry where
    columnBuilder (Contrib p t y) = mconcat [ columnBuilder p
                                            , columnBuilder t
                                            , B.fromString (last2 $ show y)
                                            ]
        where
              last2 []        = []
              last2 xs@[_]    = xs
              last2 xs@[_, _] = xs
              last2 (_:xs)    = last2 xs

instance FromNamedRecord ContribEntry where
    parseNamedRecord m =   Contrib
                       <$> m CSV..: "RecipientCandidateParty"   -- 40
                       <*> m CSV..: "DonorCommitteeType"        -- 67
                       <*> m CSV..: "ElectionCycle"             --  2

instance ToNamedRecord ContribEntry where
    toNamedRecord (Contrib p t y) =
        namedRecord [ "RecipientCandidateParty" CSV..= toField p
                    , "DonorCommitteeType"      CSV..= toField t
                    , "ElectionCycle"           CSV..= toField y
                    ]

instance ToJSON ContribEntry where
    toJSON = genericToJSON defaultOptions
