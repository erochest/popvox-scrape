{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module PopVox.Types.Common
    ( Header
    , HeaderSet

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
type OrgName         = T.Text
type Year            = Int
type State           = T.Text
type StateCode       = Int
type District        = Int
type Score           = Double
type LegislatorIndex = M.HashMap Thomas ICPSR


data PopVoxOptions
    = Transform { maplightDataDir :: !FilePath
                , maplightAPIDir  :: !FilePath
                , outputFile      :: !FilePath
                }
    | RankBills { rankBillScores :: !FilePath
                , rankBillBills  :: !FilePath
                , rankBillIndex  :: !FilePath
                , rankBillOutput :: !FilePath
                }
    | TestJson  { maplightAPIDir :: !FilePath }
    | TestCsv   { maplightDataDir :: !FilePath }
    | SearchPosition { maplightAPIDir :: !FilePath
                     , maplightOrg    :: !T.Text
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
        = AmericanInd
        | Citizens
        | Conservative
        | Constitution
        | Constitutional
        | DemFarmLabor
        | Dem
        | GrandOld
        | Green
        | Independent
        | IndAm
        | Libertarian
        | NoParty
        | Other
        | Peace
        | Reform
        | Socialist
        | Taxpayers
        | TermLimits
        | GOP
        | UnknownParty
        | WorkingFamilies
        | WriteIn
        deriving (Show, Eq, Enum, Bounded, Ord, Generic)

instance Hashable Party

instance ColumnHead Party where
    columnValue   AmericanInd     = "AmI"
    columnValue   Citizens        = "Cit"
    columnValue   Conservative    = "Con"
    columnValue   Constitution    = "Cst"
    columnValue   Constitutional  = "Csl"
    columnValue   DemFarmLabor    = "DFL"
    columnValue   Dem             = "Dem"
    columnValue   GrandOld        = "GOl"
    columnValue   Green           = "Grn"
    columnValue   Independent     = "Ind"
    columnValue   IndAm           = "IAm"
    columnValue   Libertarian     = "Lib"
    columnValue   NoParty         = "Non"
    columnValue   Other           = "Oth"
    columnValue   Peace           = "P&F"
    columnValue   Reform          = "Ref"
    columnValue   Socialist       = "Soc"
    columnValue   Taxpayers       = "Tax"
    columnValue   TermLimits      = "Trm"
    columnValue   GOP             = "GOP"
    columnValue   UnknownParty    = "Unk"
    columnValue   WorkingFamilies = "Wrk"
    columnValue   WriteIn         = "Wri"

    columnBuilder AmericanInd     = "AmI"
    columnBuilder Citizens        = "Cit"
    columnBuilder Conservative    = "Con"
    columnBuilder Constitution    = "Cst"
    columnBuilder Constitutional  = "Csl"
    columnBuilder DemFarmLabor    = "DFL"
    columnBuilder Dem             = "Dem"
    columnBuilder GrandOld        = "GOl"
    columnBuilder Green           = "Grn"
    columnBuilder Independent     = "Ind"
    columnBuilder IndAm           = "IAm"
    columnBuilder Libertarian     = "Lib"
    columnBuilder NoParty         = "Non"
    columnBuilder Other           = "Oth"
    columnBuilder Peace           = "P&F"
    columnBuilder Reform          = "Ref"
    columnBuilder Socialist       = "Soc"
    columnBuilder Taxpayers       = "Tax"
    columnBuilder TermLimits      = "Trm"
    columnBuilder GOP             = "GOP"
    columnBuilder UnknownParty    = "Unk"
    columnBuilder WorkingFamilies = "Wrk"
    columnBuilder WriteIn         = "Wri"

instance FromField Party where
    parseField "American Independent" = pure AmericanInd
    parseField "Citizens"             = pure Citizens
    parseField "Citizens'"            = pure Citizens
    parseField "Conservative"         = pure Conservative
    parseField "Constitution"         = pure Constitution
    parseField "Constitutional"       = pure Constitutional
    parseField "Democrat-Farm-Labor"  = pure DemFarmLabor
    parseField "Democratic"           = pure Dem
    parseField "Grand Old"            = pure GrandOld
    parseField "Green"                = pure Green
    parseField "Independence"         = pure Independent
    parseField "Independent"          = pure Independent
    parseField "Independent American" = pure IndAm
    parseField "Libertarian"          = pure Libertarian
    parseField "No Party Affiliation" = pure NoParty
    parseField "No Party Preference"  = pure NoParty
    parseField "None"                 = pure NoParty
    parseField "Other"                = pure Other
    parseField "Peace and Freedom"    = pure Peace
    parseField "Reform"               = pure Reform
    parseField "Republican"           = pure GOP
    parseField "Socialist"            = pure Socialist
    parseField "Socialist USA"        = pure Socialist
    parseField "Taxpayers"            = pure Taxpayers
    parseField "Term Limits"          = pure TermLimits
    parseField "Unknown"              = pure UnknownParty
    parseField "Working Families"     = pure WorkingFamilies
    parseField "Write-In"             = pure WriteIn
    parseField "\\N"                  = pure UnknownParty
    parseField ""                     = pure UnknownParty
    parseField p                      = fail $ "Invalid Party: " ++ show p

instance ToField Party where
    toField AmericanInd     = "American Independent"
    toField Citizens        = "Citizens"
    toField Conservative    = "Conservative"
    toField Constitution    = "Constitution"
    toField Constitutional  = "Constitutional"
    toField DemFarmLabor    = "Democrat-Farm-Labor"
    toField Dem             = "Democratic"
    toField GrandOld        = "Grand Old"
    toField Green           = "Green"
    toField Independent     = "Independent"
    toField IndAm           = "Independent American"
    toField Libertarian     = "Libertarian"
    toField NoParty         = "None"
    toField Other           = "Other"
    toField Peace           = "Peace and Freedom"
    toField Reform          = "Reform"
    toField Socialist       = "Socialist"
    toField Taxpayers       = "Taxpayers"
    toField TermLimits      = "TermLimits"
    toField GOP             = "Republican"
    toField UnknownParty    = "Unknown"
    toField WorkingFamilies = "Working Families"
    toField WriteIn         = "Write-In"

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
