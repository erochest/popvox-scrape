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
import           Data.Time
import qualified Data.Vector                     as V

import           PopVox.OpenSecrets.Types.Common


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

