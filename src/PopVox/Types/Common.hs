{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module PopVox.Types.Common
    ( Header
    , HeaderSet

    , OrgID
    , OrgName
    , Year
    , State
    , StateCode
    , District
    , Score

    , PopVoxOptions(..)
    , ColumnHead(..)
    , HashIndex(..)
    , Party(..)
    , Session(..)
    , Disposition(..)

    , Thomas(..)
    , ICPSR(..)
    , LegislatorIndex

    , WithHeader(..)
    ) where


import           Control.Applicative
import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Types          (defaultOptions)
import           Data.ByteString.Lazy      (ByteString)
import           Data.Csv                  hiding ((.:))
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Buildable
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Builder    as B
import           Data.Traversable
import qualified Data.Vector               as V
import           Filesystem.Path.CurrentOS hiding (concat)
import           GHC.Generics
import           Prelude                   hiding (FilePath, concat)

import           PopVox.Utils


type HeaderSet       = S.HashSet ByteString
type OrgID           = T.Text
type OrgName         = T.Text
type Year            = Int
type State           = T.Text
type StateCode       = Int
type District        = Int
type Score           = Double
type LegislatorIndex = M.HashMap Thomas ICPSR


data PopVoxOptions
    = Transform { contribDataFile  :: !FilePath
                , maplightAPIDir   :: !FilePath
                , outputFile       :: !FilePath
                }
    | RankBills { rankBillScores :: !FilePath
                , rankBillBills  :: !FilePath
                , rankBillIndex  :: !FilePath
                , rankBillOutput :: !FilePath
                }
    | TestJson  { maplightAPIDir  :: !FilePath }
    | TestCsv   { contribDataFile :: !FilePath
                , failFast        :: !Bool
                }
    | SearchPosition { maplightAPIDir :: !FilePath
                     , maplightOrg    :: !T.Text
                     }
    | ReportOn  { reportCsvFile :: !FilePath
                , reportTarget  :: !T.Text
                }
    deriving (Show)


class ColumnHead c where
    columnValue   :: c -> T.Text
    columnBuilder :: c -> B.Builder
    columnValue   = TL.toStrict . B.toLazyText . columnBuilder
    columnBuilder = B.fromText . columnValue


newtype HashIndex k v
    = HashIndex { unIndex :: M.HashMap k v }
    deriving (Show, Eq, Functor, Foldable, Traversable, NFData)

instance (Hashable k, Eq k, Monoid v) => Monoid (HashIndex k v) where
    mempty = HashIndex mempty
    mappend (HashIndex a) (HashIndex b) = HashIndex $ M.unionWith mappend a b


data Party
        = Dem
        | GOP
        | Independent
        | Unknown
        deriving (Show, Eq, Enum, Bounded, Ord, Generic)

instance Hashable Party

instance ColumnHead Party where
    columnValue   Dem         = "Dem"
    columnValue   GOP         = "GOP"
    columnValue   Independent = "Ind"
    columnValue   Unknown     = "UNK"

    columnBuilder Dem         = "Dem"
    columnBuilder GOP         = "GOP"
    columnBuilder Independent = "Ind"
    columnBuilder Unknown     = "UNK"

instance FromField Party where
    parseField "100" = pure Dem
    parseField "200" = pure GOP
    parseField "328" = pure Independent
    -- This catch-all is probably a bad idea....
    parseField _     = pure Unknown

instance ToField Party where
    toField Dem         = "100"
    toField GOP         = "200"
    toField Independent = "328"
    toField Unknown     = "UNK"

instance ToJSON Party where
    toJSON = genericToJSON defaultOptions


newtype Session  = Session { getSession :: Int }
                   deriving (Eq, Enum, Real, Integral, Num, Ord, Generic)

instance Hashable Session

instance Show Session where
    show (Session s) = show s

instance FromJSON Session where
    parseJSON (Number s) = pure $ truncate s
    parseJSON (String s) = Session <$> parseInt s
    parseJSON s          = fail $ "Invalid Session: " ++ show s

instance ToJSON Session where
    toJSON = Number . fromIntegral . getSession

instance Buildable Session where
    build (Session s) = build s


newtype Thomas = Thomas { unThomas :: T.Text }
                 deriving (Show, Eq, Generic)

instance Hashable Thomas

instance FromJSON Thomas where
    parseJSON (Object o) = Thomas <$> o .: "thomas_id"
    parseJSON v          = fail $ "Invalid legislator information: " ++ show v


newtype ICPSR = ICPSR { unICPSR :: Int }
                deriving (Eq, Show, Generic)

instance Hashable ICPSR


data Disposition = Support
                 | Neutral
                 | Oppose
                 deriving (Show, Eq, Generic)

instance Monoid Disposition where
    mempty = Neutral
    mappend a b = toEnum $ fromEnum a + fromEnum b

instance Enum Disposition where
    toEnum n | n < 0     = Oppose
             | n == 0    = Neutral
             | n > 0     = Support
             | otherwise = error $ "Invalid enum for Disposition: " ++ show n

    fromEnum Support = 1
    fromEnum Neutral = 0
    fromEnum Oppose  = -1

instance FromJSON Disposition where
    parseJSON (String "support") = pure Support
    parseJSON (String "Null")    = pure Neutral
    parseJSON (String "oppose")  = pure Oppose
    parseJSON Null               = pure Neutral
    parseJSON d                  = fail $ "Invalid Disposition: " ++ show d

instance ToJSON Disposition where
    toJSON = genericToJSON defaultOptions


data WithHeader a = WithHeader Header a

instance ToNamedRecord a => ToNamedRecord (WithHeader a) where
    toNamedRecord (WithHeader header a) =
        V.foldl' ensure (toNamedRecord a) header
        where
            ensure :: NamedRecord -> Name -> NamedRecord
            ensure m n | n `M.member` m = m
                       | otherwise      = M.insert n mempty m
