{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}


module PopVox.OpenSecrets.Types.Committees
    ( CommitteeRecord(..)
    , indexCmte
    ) where


import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Data.CSV.Conduit.Conversion
import qualified Data.DList                          as D
import qualified Data.HashMap.Strict                 as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                           as T
import           Data.Text.Encoding                  (decodeLatin1)
import           Data.Traversable
import           Data.Tuple
import qualified Data.Vector                         as V

import           PopVox.OpenSecrets.Types.Common
import           PopVox.OpenSecrets.Types.Individual
import           PopVox.Types


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

indexCmte :: HashIndex T.Text (D.DList Individual)
          -> Either String CommitteeRecord
          -> OrgIndex Int
indexCmte _      (Left _)       = HashIndex M.empty
indexCmte indivs (Right CR{..}) = HashIndex
                                . M.singleton _crUltOrg
                                . HashIndex
                                . M.fromListWith mappend
                                . mapMaybe ( fmap swap
                                           . sequenceA
                                           . ((Sum . _indAmount) &&& (getParty' . _indRecipCode)))
                                . maybe [] D.toList
                                . M.lookup _crID
                                $ getIndex indivs
