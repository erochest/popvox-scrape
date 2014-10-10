{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module PopVox.OpenSecrets.Types.PACCandidate
    ( PACCandidate(..)
    ) where


import           Control.Applicative
import qualified Data.ByteString                 as B
import           Data.CSV.Conduit.Conversion
import qualified Data.Text                       as T
import           Data.Time
import qualified Data.Vector                     as V

import           PopVox.OpenSecrets.Types.Common


data PACCandidate = PACCandidate
                  { _paccCycle       :: !Year
                  , _paccFECID       :: !T.Text
                  , _paccPACID       :: !T.Text
                  , _paccCandidateID :: !T.Text
                  , _paccAmount      :: !Int
                  , _paccDate        :: !(Maybe Day)
                  , _paccRealCode    :: !T.Text
                  , _paccType        :: !TransactionType
                  , _paccDirect      :: !Bool
                  , _paccFECCandID   :: !T.Text
                  } deriving (Show, Eq)


instance FromRecord PACCandidate where
    parseRecord r
        | V.length r == 10 =   PACCandidate
                           <$> r .! 0
                           <*> r .! 1
                           <*> r .! 2
                           <*> r .! 3
                           <*> r .! 4
                           <*> optional (r .! 5)
                           <*> r .! 6
                           <*> r .! 7
                           <*> ((== ("D" :: B.ByteString)) <$> r .! 8)
                           <*> r .! 9
        | otherwise        = fail "Invalid PACCandidate"
