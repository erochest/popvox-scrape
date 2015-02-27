{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module PopVox.Types.Bills
    ( Bill(..)
    , BillNo(..)
    , BillType(..)
    , BillSponsors(..)

    , BillInfo(..)
    , billInfoBill
    , billInfoOrgs

    , BillIndex
    ) where


import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types           (defaultOptions)
import           Data.Csv                   hiding ((.:))
import           Data.Hashable
import           Data.Monoid
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.Format           as F
import           GHC.Generics

import           PopVox.Types.Common
import           PopVox.Types.Orgs
import           PopVox.Utils


type BillIndex = HashIndex Bill Disposition

newtype BillNo = BillNo { getBillNo :: Int }
                 deriving (Show, Eq, Enum, Real, Integral, Num, Ord, Generic)

instance Hashable BillNo

instance FromJSON BillNo where
    parseJSON (Number s) = pure $ truncate s
    parseJSON (String s) = BillNo <$> parseInt s
    parseJSON s          = fail $ "Invalid BillNo: " ++ show s

instance ToJSON BillNo where
    toJSON = Number . fromIntegral . getBillNo


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
    columnBuilder (Bill n ch cong) =
        columnBuilder ch <> F.build "{} ({})" (getBillNo n, cong)

instance ToField Bill where
    toField = encodeUtf8 . columnValue

instance ToJSON Bill where
    toJSON = genericToJSON defaultOptions


data BillSponsors s = BillSponsors
                    { billSponsorInfo :: !Bill
                    , billSponsor     :: !(Maybe s)
                    , billCosponsors  :: ![s]
                    } deriving (Eq, Show, Generic)

instance Functor BillSponsors where
    fmap f (BillSponsors bill s ss) = BillSponsors bill (fmap f s) (fmap f ss)

instance FromJSON (BillSponsors Thomas) where
    parseJSON (Object o) =   BillSponsors
                         <$> bill o
                         <*> optional (o .: "sponsor" >>= parseJSON)
                         <*> (o .: "cosponsors" >>= parseJSON)
        where
            bill b =   Bill
                   <$> (b .: "number"    >>= parseJSON)
                   <*> (b .: "bill_type" >>= bill_type)
                   <*> (b .: "congress"  >>= parseJSON)
            bill_type (String "hr")      = pure House
            bill_type (String "hres")    = pure HouseResolution
            bill_type (String "hconres") = pure HouseConcurrent
            bill_type (String "hjres")   = pure HouseJoint
            bill_type (String "s")       = pure Senate
            bill_type (String "sres")    = pure SenateResolution
            bill_type (String "sconres") = pure SenateConcurrent
            bill_type (String "sjres")   = pure SenateJoint
            bill_type bt                 = fail $ "Invalid BillType: " ++ show bt
    parseJSON b          = fail $ "Invalid BillSponsors: " ++ show b


data BillInfo = BillInfo !Bill ![OrgInfo]
              deriving (Show, Generic)

billInfoBill :: Lens' BillInfo Bill
billInfoBill f (BillInfo b os) = fmap (`BillInfo` os) (f b)

billInfoOrgs :: Lens' BillInfo [OrgInfo]
billInfoOrgs f (BillInfo b os) = fmap (BillInfo b) (f os)

instance FromJSON BillInfo where
    parseJSON v@(Object o) =   BillInfo
                           <$> parseJSON v
                           <*> o .: "organizations"
    parseJSON b            = fail $ "Invalid BillInfo: " ++ show b

instance ToJSON BillInfo where
    toJSON = genericToJSON defaultOptions
