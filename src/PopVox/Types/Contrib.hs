{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module PopVox.Types.Contrib
    ( Amount
    , ContribInfo
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
import qualified Data.HashSet           as S
import           Data.Monoid
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as B
import           GHC.Generics

import           PopVox.Types.Common


type Amount        = Double
type DistrictName  = T.Text
type ContribInfo   = (First DistrictName, Sum Amount)
type ContribInfo'  = (Maybe DistrictName, Amount)
type ContribIndex  = HashIndex ContribEntry ContribInfo
type ContribIndex' = HashIndex ContribEntry ContribInfo'

data ContribEntry
        = Candidate
        { contribYear  :: !Year
        , contribParty :: !Party
        }
        | Committee
        { contribYear  :: !Year
        }
        | StateLevel
        { contribYear  :: !Year
        , stateCode    :: !T.Text
        }
        deriving (Show, Eq, Ord, Generic)

instance Hashable ContribEntry

last2 :: [a] -> [a]
last2 []        = []
last2 xs@[_]    = xs
last2 xs@[_, _] = xs
last2 (_:xs)    = last2 xs

instance ColumnHead ContribEntry where
    columnBuilder (Candidate y p)   = mconcat [ columnBuilder p
                                              , B.fromString (last2 $ show y)
                                              ]
    columnBuilder (Committee y)     = mconcat [ ""
                                              , B.fromString (last2 $ show y)
                                              ]
    columnBuilder (StateLevel y st) = mconcat [ B.fromText st
                                              , B.fromString (last2 $ show y)
                                              ]

stateCodes :: S.HashSet T.Text
stateCodes = S.fromList [ "AL" , "AK" , "AZ" , "AR" , "CA" , "CO"
                        , "CT" , "DE" , "DC" , "FL" , "GA" , "HI"
                        , "ID" , "IL" , "IN" , "IA" , "KS" , "KY"
                        , "LA" , "ME" , "MD" , "MA" , "MI" , "MN"
                        , "MS" , "MO" , "MT" , "NE" , "NV" , "NH"
                        , "NJ" , "NM" , "NY" , "NC" , "ND" , "OH"
                        , "OK" , "OR" , "PA" , "RI" , "SC" , "SD"
                        , "TN" , "TX" , "UT" , "VT" , "VA" , "WA"
                        , "WV" , "WI" , "WY"
                        ]

instance FromNamedRecord ContribEntry where
    parseNamedRecord m = do
        rtype <- defaulting "CAND" <$> (m CSV..: "recipient_type" :: Parser T.Text)
        case rtype of
            "CAND" -> Candidate <$> m CSV..: "cycle"
                                <*> m CSV..: "recipient_party"
            "COMM" -> Committee <$> m CSV..: "cycle"
            r | r `S.member` stateCodes -> StateLevel <$> m CSV..: "cycle"
                                                      <*> pure r
              | otherwise -> fail $ "Invalid recipient_type: " ++ T.unpack r
        where
            defaulting d v | T.null v  = d
                           | otherwise = v

instance ToNamedRecord ContribEntry where
    toNamedRecord (Candidate y p) =
        namedRecord [ "recipient_type"  CSV..= ("CAND" :: T.Text)
                    , "recipient_party" CSV..= toField p
                    , "cycle"           CSV..= toField y
                                    ]
    toNamedRecord (Committee y) =
        namedRecord [ "recipient_type" CSV..= ("COMM" :: T.Text)
                    , "cycle"          CSV..= toField y
                    ]
    toNamedRecord (StateLevel y st) =
        namedRecord [ "recipient_type" CSV..= st
                    , "cycle"          CSV..= toField y
                    ]

instance ToJSON ContribEntry where
    toJSON = genericToJSON defaultOptions
