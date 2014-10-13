{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module PopVox.OpenSecrets.Types.Expenditures
    ( Expenditure(..)
    ) where


import Data.Text.Encoding
import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as C8
import           Data.CSV.Conduit.Conversion
import qualified Data.Text                       as T
import           Data.Time
import qualified Data.Vector                     as V

import           PopVox.OpenSecrets.Types.Common


data RecipientEntity = Candidate
                     | CandidateCmte
                     | RecipCommittee
                     | PartyCmte
                     | RecipPAC
                     | Organization
                     | Individual
                     deriving (Show, Eq)
makePrisms ''RecipientEntity

instance FromField RecipientEntity where
    parseField "CAN" = pure Candidate
    parseField "CCM" = pure CandidateCmte
    parseField "COM" = pure RecipCommittee
    parseField "PTY" = pure PartyCmte
    parseField "PAC" = pure RecipPAC
    parseField "ORG" = pure Organization
    parseField "IND" = pure Individual
    parseField r     = fail $ "Invalid RecipientEntity: '" ++ C8.unpack r ++ "'"

data Expenditure = Expenditure
                 { _expCycle         :: !Year
                 , _expID            :: !T.Text
                 , _expTransID       :: !T.Text
                 , _expCRPFilerID    :: !T.Text
                 , _expRecipCode     :: !(Maybe RecipientType)
                 , _expPACShort      :: !T.Text
                 , _expCRPRecipName  :: !T.Text
                 , _expExpCode       :: !T.Text
                 , _expAmount        :: !Double
                 , _expDate          :: !(Maybe Day)
                 , _expCity          :: !T.Text
                 , _expState         :: !T.Text
                 , _expZip           :: !T.Text
                 , _expCmteID_EF     :: !T.Text
                 , _expCandID        :: !T.Text
                 , _expType          :: !(Maybe TransactionType)
                 , _expDescription   :: !T.Text
                 , _expElection      :: !ElectionType
                 , _expElectionOther :: !T.Text
                 , _expEntType       :: !(Maybe RecipientEntity)
                 , _expSource        :: !T.Text
                 } deriving (Show, Eq)
makeLenses ''Expenditure

instance FromRecord Expenditure where
    parseRecord r
        | V.length r == 21 =   Expenditure
                           <$> r .! 0
                           <*> r .! 1
                           <*> r .! 2
                           <*> r .! 3
                           <*> r .! 4
                           <*> r .! 5
                           <*> (decodeLatin1 <$> r .! 6)
                           <*> r .! 7
                           <*> r .! 8
                           <*> r .! 9
                           <*> (decodeLatin1 <$> r .! 10)
                           <*> r .! 11
                           <*> r .! 12
                           <*> r .! 13
                           <*> r .! 14
                           <*> r .! 15
                           <*> r .! 16
                           <*> r .! 17
                           <*> r .! 18
                           <*> optional (r .! 19)
                           <*> r .! 20
        | otherwise        = fail $ "Invalid Expenditure: " ++ show r ++ "'"
