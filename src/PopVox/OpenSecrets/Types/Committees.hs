{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module PopVox.OpenSecrets.Types.Committees
    ( CommitteeRecord(..)
    ) where


import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString.Char8           as C8
import           Data.CSV.Conduit.Conversion
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeLatin1)
import qualified Data.Vector                     as V

import           PopVox.OpenSecrets.Types.Common


data CommitteeRecord = CR
                     { _crCycle     :: !Year
                     , _crID        :: !T.Text
                     , _crPACShort  :: !T.Text
                     , _crAffliate  :: !(Maybe T.Text)
                     , _crUltOrg    :: !T.Text
                     , _crRecipID   :: !T.Text
                     , _crRecipCode :: !(Maybe RecipientType)
                     , _crFEDCandID :: !T.Text
                     , _crParty     :: !(Maybe Party)
                     , _crPrimCode  :: !T.Text
                     , _crSource    :: !T.Text
                     , _crSensitive :: !Bool
                     , _crForeign   :: !Bool
                     , _crActive    :: !Bool
                     } deriving (Show, Eq)
makeLenses ''CommitteeRecord

instance FromRecord CommitteeRecord where
    parseRecord r
        | V.length r == 14 =   CR
                           <$> r .! 0
                           <*> r .! 1
                           <*> r .! 2
                           <*> r .! 3
                           <*> (decodeLatin1 <$> r .! 4)
                           <*> r .! 5
                           <*> r .! 6
                           <*> r .! 7
                           <*> (r .! 8 <|> pure Nothing)
                           <*> r .! 9
                           <*> r .! 10
                           <*> r .! 11
                           <*> r .! 12
                           <*> r .! 13
        | otherwise        =   fail "Invalid CommitteeRecord"
