{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module PopVox.OpenSecrets.Types.Individual
    ( Individual(..)
    ) where


import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString.Char8           as C8
import           Data.CSV.Conduit.Conversion
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeLatin1)
import qualified Data.Vector                     as V
import Data.Time
import System.Locale

import           PopVox.OpenSecrets.Types.Common


instance FromField Day where
    parseField bs =
        maybe err pure $ parseTime defaultTimeLocale "%m/%d/%Y" (C8.unpack bs)
        where err = fail $ "Invalid date: '" ++ C8.unpack bs ++ "'"

data TransactionType = TransContribution
                     | TransEarmarked
                     | TransCommittee
                     | TransRefund
                     | TransSoft
                     | TransCode T.Text
                     deriving (Show, Eq)

instance FromField TransactionType where
    parseField "15"  = pure TransContribution
    parseField "15 " = pure TransContribution
    parseField "15e" = pure TransEarmarked
    parseField "15E" = pure TransEarmarked
    parseField "15j" = pure TransCommittee
    parseField "15J" = pure TransCommittee
    parseField "22y" = pure TransRefund
    parseField "22Y" = pure TransRefund
    parseField "10"  = pure TransSoft
    parseField "10 " = pure TransSoft
    parseField t     = pure . TransCode $ decodeLatin1 t

data Gender = Male
            | Female
            | NameAmbiguous
            | GenderUnknown
            deriving (Show, Eq)
makePrisms ''Gender

instance FromField Gender where
    parseField "M" = pure Male
    parseField "F" = pure Female
    parseField "N" = pure NameAmbiguous
    parseField "U" = pure GenderUnknown
    parseField g   = fail $ "Invalid Gender: '" ++ C8.unpack g ++ "'"

data Individual = Individual
                { _indCycle          :: !Year
                , _indFECID          :: !T.Text
                , _indContribID      :: !T.Text
                , _indContrib        :: !T.Text
                , _indRecipID        :: !T.Text
                , _indOrgName        :: !T.Text
                , _indUltOrg         :: !T.Text
                , _indRealCode       :: !T.Text
                , _indDate           :: !(Maybe Day)
                , _indAmount         :: !Int
                , _indStreet         :: !T.Text
                , _indCity           :: !T.Text
                , _indState          :: !T.Text
                , _indZip            :: !T.Text
                , _indRecipCode      :: !(Maybe RecipientType)
                , _indType           :: !TransactionType
                , _indCommitteeID    :: !T.Text
                , _indOtherID        :: !T.Text
                , _indGender         :: !(Maybe Gender)
                , _indMicrofilm      :: !T.Text
                , _indOccupation     :: !T.Text
                , _indEmployer       :: !T.Text
                , _indRealCodeSource :: !T.Text
                } deriving (Show, Eq)
makeLenses ''Individual

instance FromRecord Individual where
    parseRecord r
        | V.length r == 23 =   Individual
                           <$> r .! 0
                           <*> r .! 1
                           <*> r .! 2
                           <*> (decodeLatin1 <$> r .! 3)
                           <*> r .! 4
                           <*> (decodeLatin1 <$> r .! 5)
                           <*> (decodeLatin1 <$> r .! 6)
                           <*> r .! 7
                           <*> optional (r .! 8)
                           <*> r .! 9
                           <*> (decodeLatin1 <$> r .! 10)
                           <*> r .! 11
                           <*> r .! 12
                           <*> r .! 13
                           <*> optional (r .! 14)
                           <*> r .! 15
                           <*> r .! 16
                           <*> r .! 17
                           <*> optional (r .! 18)
                           <*> r .! 19
                           <*> r .! 20
                           <*> (decodeLatin1 <$> r .! 21)
                           <*> r .! 22
        | otherwise        = fail "Invalid Individual"

