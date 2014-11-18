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

    , WithHeader(..)

    ) where


import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types           (defaultOptions)
import           Data.Bifunctor             (bimap, first)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import           Data.ByteString.Lazy       (ByteString)
import           Data.Csv                   hiding ((.:))
import qualified Data.Csv                   as CSV
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as S
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Text.Buildable
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as B
import           Data.Text.Lazy.Builder.Int
import qualified Data.Text.Read             as TR
import           Data.Traversable
import qualified Data.Vector                as V
import           Filesystem.Path.CurrentOS  hiding (concat)
import           GHC.Generics
import           Prelude                    hiding (FilePath, concat)


type ApiKey = T.Text

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


data OrgContrib = OrgContrib !OrgName !ContribEntry !Int
                deriving (Show, Eq)

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
billsRecord = map (columnbs `bimap` (showbs . fromEnum)) . M.toList

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

data WithHeader a = WithHeader Header a

instance ToNamedRecord a => ToNamedRecord (WithHeader a) where
    toNamedRecord (WithHeader header a) =
        V.foldl' ensure (toNamedRecord a) header
        where
            ensure :: NamedRecord -> Name -> NamedRecord
            ensure m n | n `M.member` m = m
                       | otherwise      = M.insert n mempty m


data PopVoxOptions
    = Transform
    { maplightDataDir :: !FilePath
    , maplightAPIDir  :: !FilePath
    , outputFile      :: !FilePath
    }
    | TestJson { maplightAPIDir :: !FilePath }
    | TestCsv  { maplightDataDir :: !FilePath }
    deriving (Show)
