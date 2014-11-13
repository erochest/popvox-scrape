{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module PopVox.Types
    ( ApiKey
    , Header

    , PopVoxOptions(..)

    , ColumnHead(..)

    , HashIndex(..)

    , OrgName
    , Year
    , Party(..)
    , ContribType(..)
    , ContribEntry(..)
    , ContribIndex

    , Chamber(..)
    , Congress
    , BillNo
    , Bill(..)
    , Position
    , BillIndex

    , OrgData(..)

    ) where


import           Control.DeepSeq
import           Data.ByteString            (ByteString)
import           Data.CSV.Conduit           (Row)
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict        as M
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as B
import           Data.Text.Lazy.Builder.Int
import           Data.Traversable
import           GHC.Generics


type ApiKey = T.Text

type Header = Row ByteString

type OrgName  = T.Text
type Year     = Int
type Congress = Int
type BillNo   = Int
type Position = Int

type ContribIndex = HashIndex ContribEntry Int
type BillIndex    = HashIndex Bill Int


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


data ContribType = Contribution
                 | Expenditure
                 deriving (Show, Eq, Enum, Ord, Bounded, Generic)

instance Hashable ContribType

instance ColumnHead ContribType where
    columnValue   Expenditure  = "Ind"
    columnValue   Contribution = ""
    columnBuilder Expenditure  = "Ind"
    columnBuilder Contribution = ""


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


data Chamber = House
             | Senate
             deriving (Eq, Enum, Bounded, Ord, Generic)

instance ColumnHead Chamber where
    columnValue   House  = "HR"
    columnValue   Senate = "S"
    columnBuilder House  = "HR"
    columnBuilder Senate = "S"


data Bill = Bill
          { billNo       :: !BillNo
          , billChamber  :: !Chamber
          , billCongress :: !Congress
          } deriving (Eq, Ord, Generic)

instance ColumnHead Bill where
    columnBuilder (Bill n ch cong) = mconcat [ columnBuilder ch
                                             , decimal n
                                             , " ("
                                             , decimal cong
                                             , B.singleton ')'
                                             ]


data OrgData = Org
             { orgName     :: !OrgName
             , orgContribs :: !ContribIndex
             , orgBills    :: !BillIndex
             }

data PopVoxOptions = PopVoxOptions
                   { maplightApiKey :: ApiKey
                   } deriving (Show)
