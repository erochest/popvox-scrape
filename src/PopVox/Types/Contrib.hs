{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module PopVox.Types.Contrib
    ( Contrib
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
import qualified Data.Text              as T
import qualified Data.Text.Lazy.Builder as B
import           GHC.Generics

import           PopVox.Types.Common


type Contrib       = Double
type ContribIndex  = HashIndex ContribEntry (Sum Contrib)
type ContribIndex' = HashIndex ContribEntry Contrib

data ContribEntry
        = Candidate
        { contribYear  :: !Year
        , contribParty :: !Party
        }
        | Committee
        { contribYear :: !Year
        }
        deriving (Show, Eq, Ord, Generic)

instance Hashable ContribEntry

last2 :: [a] -> [a]
last2 []        = []
last2 xs@[_]    = xs
last2 xs@[_, _] = xs
last2 (_:xs)    = last2 xs

instance ColumnHead ContribEntry where
    columnBuilder (Candidate y p) = mconcat [ columnBuilder p
                                            , B.fromString (last2 $ show y)
                                            ]
    columnBuilder (Committee y)   = mconcat [ ""
                                            , B.fromString (last2 $ show y)
                                            ]

instance FromNamedRecord ContribEntry where
    parseNamedRecord m = do
        rtype <- defaulting "CAND" <$> (m CSV..: "recipient_type" :: Parser T.Text)
        case rtype of
            "CAND" -> Candidate <$> m CSV..: "cycle"
                                <*> m CSV..: "recipient_party"
            "COMM" -> Committee <$> m CSV..: "cycle"
            r      -> fail $ "Invalid recipient_type: " ++ T.unpack r
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

instance ToJSON ContribEntry where
    toJSON = genericToJSON defaultOptions
