{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module PopVox.Types
    ( ApiKey
    , Header
    , HeaderSet

    , PopVoxOptions(..)

    , ColumnHead(..)

    , HashIndex(..)

    , OrgName
    , Year
    , Party(..)
    , ContribType(..)
    , ContribEntry(..)
    , ContribIndex
    , ContribIndex'
    , Disposition(..)

    , BillType(..)
    , Session(..)
    , BillNo(..)
    , Bill(..)
    , Position
    , BillIndex

    , OrgInfo(..)
    , BillInfo(..)

    , OrgData(..)
    , OrgContrib(..)
    , OrgBillIndex
    , OrgContribIndex
    , OrgContribIndex'

    ) where


import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types            (defaultOptions)
import           Data.ByteString             (ByteString)
import           Data.CSV.Conduit            (Row)
import           Data.CSV.Conduit.Conversion hiding ((.:))
import qualified Data.CSV.Conduit.Conversion as CSV
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict         as M
import qualified Data.HashSet                as S
import           Data.Monoid
import qualified Data.Text                   as T
import           Data.Text.Buildable
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Builder      as B
import           Data.Text.Lazy.Builder.Int
import qualified Data.Text.Read              as TR
import           Data.Traversable
import           Filesystem.Path.CurrentOS
import           GHC.Generics
import           Prelude                     hiding (FilePath)


type ApiKey = T.Text

type Header    = Row ByteString
type HeaderSet = S.HashSet ByteString

type OrgName  = T.Text
type Year     = Int
type Position = Int

newtype Session  = Session { getSession :: Int }
                   deriving (Eq, Enum, Real, Integral, Num, Ord, Generic)

instance Hashable Session

instance Show Session where
    show (Session s) = show s

parseInt :: Monad m => T.Text -> m Int
parseInt t = case TR.decimal t of
                 Right (n, "") -> return n
                 Right (_, xs) -> fail . T.unpack $ "Extra characters: " <> xs
                 Left err      -> fail err

instance FromJSON Session where
    parseJSON (Number s) = pure $ truncate s
    parseJSON (String s) = Session <$> parseInt s
    parseJSON s          = fail $ "Invalid Session: " ++ show s

instance ToJSON Session where
    toJSON = Number . fromIntegral . getSession

instance Buildable Session where
    build (Session s) = build s

newtype BillNo   = BillNo { getBillNo :: Int }
                   deriving (Show, Eq, Enum, Real, Integral, Num, Ord, Generic)

instance Hashable BillNo

instance FromJSON BillNo where
    parseJSON (Number s) = pure $ truncate s
    parseJSON (String s) = BillNo <$> parseInt s
    parseJSON s          = fail $ "Invalid BillNo: " ++ show s

instance ToJSON BillNo where
    toJSON = Number . fromIntegral . getBillNo

type ContribIndex     = HashIndex ContribEntry (Sum Int)
type ContribIndex'    = HashIndex ContribEntry Int
type BillIndex        = M.HashMap Bill Disposition
type OrgBillIndex     = HashIndex OrgName BillIndex
type OrgContribIndex  = HashIndex OrgName ContribIndex
type OrgContribIndex' = HashIndex OrgName ContribIndex'


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


data Party = Dem | GOP
           deriving (Show, Eq, Enum, Bounded, Ord, Generic)

instance Hashable Party

instance ColumnHead Party where
    columnValue   Dem = "Dem"
    columnValue   GOP = "GOP"
    columnBuilder Dem = "Dem"
    columnBuilder GOP = "GOP"

instance FromField Party where
    parseField "Democratic" = pure Dem
    parseField "Republican" = pure GOP
    parseField p            = fail $ "Invalid Party: " ++ show p

instance ToField Party where
    toField Dem = "Democratic"
    toField GOP = "Republican"

instance ToJSON Party where
    toJSON = genericToJSON defaultOptions


data ContribType = Contribution
                 | Expenditure
                 deriving (Show, Eq, Enum, Ord, Bounded, Generic)

instance Hashable ContribType

instance FromField ContribType where
    parseField "Independent Expenditor (Person or Group)" = pure Expenditure
    parseField "Single Candidate Independent Expenditure" = pure Expenditure
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
                  { contribParty  :: !Party
                  , contribType   :: !ContribType
                  , contribYear   :: !Year
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
                       <$> m CSV..: "RecipientCandidateParty"
                       <*> m CSV..: "DonorCommitteeType"
                       <*> m CSV..: "ElectionCycle"

instance ToNamedRecord ContribEntry where
    toNamedRecord (Contrib p t y) =
        namedRecord [ "RecipientCandidateParty" CSV..= toField p
                    , "DonorCommitteeType"      CSV..= toField t
                    , "ElectionCycle"           CSV..= toField y
                    ]

instance ToJSON ContribEntry where
    toJSON = genericToJSON defaultOptions


data OrgContrib = OrgContrib !OrgName !ContribEntry !Int
                deriving (Show, Eq)

instance FromNamedRecord OrgContrib where
    parseNamedRecord m =   OrgContrib
                       <$> m CSV..: "DonorNameNormalized"
                       <*> parseNamedRecord m
                       <*> m CSV..: "TransactionAmount"

instance ToNamedRecord OrgContrib where
    toNamedRecord (OrgContrib n c a) =
        namedRecord [ "DonorNameNormalized" CSV..= toField n
                    , "TransactionAmount"   CSV..= toField a
                    ]
        <> toNamedRecord c


data BillType = House
              | HouseResolution
              | HouseJoint
              | HouseConcurrent
              | Senate
              | SenateResolution
              | SenateJoint
              | SenateConcurrent
              deriving (Eq, Show, Enum, Bounded, Ord, Generic)

instance Hashable BillType

instance ColumnHead BillType where

    columnValue   House            = "H"
    columnValue   HouseResolution  = "HR"
    columnValue   HouseJoint       = "HJ"
    columnValue   HouseConcurrent  = "HC"
    columnValue   Senate           = "S"
    columnValue   SenateResolution = "SR"
    columnValue   SenateJoint      = "SJ"
    columnValue   SenateConcurrent = "SC"

    columnBuilder   House            = "H"
    columnBuilder   HouseResolution  = "HR"
    columnBuilder   HouseJoint       = "HJ"
    columnBuilder   HouseConcurrent  = "HC"
    columnBuilder   Senate           = "S"
    columnBuilder   SenateResolution = "SR"
    columnBuilder   SenateJoint      = "SJ"
    columnBuilder   SenateConcurrent = "SC"

instance FromJSON BillType where
    parseJSON (String "H")  = return House
    parseJSON (String "h")  = return House
    parseJSON (String "HR") = return HouseResolution
    parseJSON (String "hr") = return HouseResolution
    parseJSON (String "HJ") = return HouseJoint
    parseJSON (String "hj") = return HouseJoint
    parseJSON (String "HC") = return HouseConcurrent
    parseJSON (String "hc") = return HouseConcurrent
    parseJSON (String "S")  = return Senate
    parseJSON (String "s")  = return Senate
    parseJSON (String "SR") = return SenateResolution
    parseJSON (String "sr") = return SenateResolution
    parseJSON (String "SJ") = return SenateJoint
    parseJSON (String "sj") = return SenateJoint
    parseJSON (String "SC") = return SenateResolution
    parseJSON (String "sc") = return SenateResolution
    parseJSON b             = fail $ "Invalid BillType: " ++ show b

instance ToJSON BillType where
    toJSON = genericToJSON defaultOptions


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


data Bill = Bill
          { billNo      :: !BillNo
          , billType    :: !BillType
          , billSession :: !Session
          } deriving (Eq, Ord, Show, Generic)

instance Hashable Bill

instance FromJSON Bill where
    parseJSON (Object o) =   Bill
                         <$> o .: "number"
                         <*> o .: "prefix"
                         <*> o .: "session"
    parseJSON b          = fail $ "Invalid Bill: " ++ show b

instance ColumnHead Bill where
    columnBuilder (Bill n ch cong) = mconcat [ columnBuilder ch
                                             , decimal n
                                             , " ("
                                             , decimal cong
                                             , B.singleton ')'
                                             ]

instance ToJSON Bill where
    toJSON = genericToJSON defaultOptions


data OrgInfo  = OrgInfo !OrgName !Disposition
              deriving (Show)

instance FromJSON OrgInfo where
    parseJSON (Object o) =   OrgInfo
                         <$> o .: "name"
                         <*> o .: "disposition"
    parseJSON o          = fail $ "Invalid OrgInfo: " ++ show o

data BillInfo = BillInfo !Bill ![OrgInfo]
              deriving (Show)

instance FromJSON BillInfo where
    parseJSON v@(Object o) =   BillInfo
                           <$> parseJSON v
                           <*> o .: "organizations"
    parseJSON b            = fail $ "Invalid BillInfo: " ++ show b


data OrgData = Org
             { orgName     :: !OrgName
             , orgContribs :: !ContribIndex
             , orgBills    :: !BillIndex
             }

data PopVoxOptions
    = Transform
    { maplightDataDir :: !FilePath
    , maplightAPIDir  :: !FilePath
    , outputFile      :: !FilePath
    }
    | TestJson
    { maplightAPIDir :: !FilePath
    }
    deriving (Show)
