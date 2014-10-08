{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module PopVox.OpenSecrets.Types.Candidates
    ( Candidate(..)
    ) where


import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString.Char8           as C8
import           Data.CSV.Conduit.Conversion
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeLatin1)
import qualified Data.Vector                     as V

import           PopVox.OpenSecrets.Types.Common


data CandidateType = CTIncumb
                   | CTChallenger
                   | CTOpenSeat
                   deriving (Show, Eq)

instance FromField CandidateType where
    parseField "I" = pure CTIncumb
    parseField "C" = pure CTChallenger
    parseField "O" = pure CTOpenSeat
    parseField f   = fail $ "Invalid CandidateType: '" ++ C8.unpack f ++ "'"

data Candidate = Candidate
               { _cCycle          :: !Year
               , _cFECID          :: !T.Text
               , _cID             :: !T.Text
               , _cName           :: !T.Text
               , _cParty          :: !Party
               , _cDistIDRunning  :: !District
               , _cDistIDCurr     :: !(Maybe District)
               , _cCurrentFederal :: !Bool
               , _cCycleFederal   :: !Bool
               , _cCandType       :: !(Maybe CandidateType)
               , _cRecipCode      :: !(Maybe RecipientType)
               -- ^ This could be more restricted than @RecipientType@ is.
               , _cNoPACS         :: !Bool
               } deriving (Show, Eq)
makeLenses ''Candidate

instance FromRecord Candidate where
    parseRecord r
        | V.length r == 12 =   Candidate
                           <$> r .! 0
                           <*> r .! 1
                           <*> r .! 2
                           <*> r .! 3
                           <*> r .! 4
                           <*> r .! 5
                           <*> r .! 6
                           <*> r .! 7
                           <*> r .! 8
                           <*> optional (r .! 9)
                           <*> r .! 10
                           <*> r .! 11
        | otherwise        = fail "Invalid Candidate"
