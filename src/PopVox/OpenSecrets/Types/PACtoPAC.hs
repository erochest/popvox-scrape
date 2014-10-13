{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module PopVox.OpenSecrets.Types.PACtoPAC
    ( PACtoPAC(..)
    ) where


import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as C8
import           Data.CSV.Conduit.Conversion
import qualified Data.Text                       as T
import           Data.Time
import qualified Data.Vector                     as V

import           PopVox.OpenSecrets.Types.Common


data PACtoPAC = PACtoPAC
              { _pacpCycle            :: !Year
              , _pacpFECRecNo         :: !T.Text
              , _pacpFilerID          :: !T.Text
              , _pacpDonorCmte        :: !T.Text
              , _pacpContribLendTrans :: !T.Text
              , _pacpCity             :: !T.Text
              , _pacpState            :: !T.Text
              , _pacpZip              :: !T.Text
              , _pacpFECOccEmp        :: !T.Text
              , _pacpPrimCode         :: !T.Text
              , _pacpDate             :: !(Maybe Day)
              , _pacpAmount           :: !Double
              , _pacpRecipID          :: !T.Text
              , _pacpParty            :: !(Maybe Party)
              , _pacpOtherID          :: !T.Text
              , _pacpRecipCode        :: !(Maybe RecipientType)
              , _pacpRecipPrimCode    :: !T.Text
              , _pacpAmend            :: !Bool
              , _pacpReport           :: !T.Text
              , _pacpElection         :: !(Maybe ElectionType)
              , _pacpMicrofilm        :: !T.Text
              , _pacpType             :: !TransactionType
              , _pacpRealCode         :: !T.Text
              , _pacpSource           :: !T.Text
              } deriving (Show, Eq)
makeLenses ''PACtoPAC

instance FromRecord PACtoPAC where
    parseRecord r
        | V.length r == 24 =   PACtoPAC
                           <$> r .! 0
                           <*> r .! 1
                           <*> r .! 2
                           <*> r .! 3
                           <*> r .! 4
                           <*> r .! 5
                           <*> r .! 6
                           <*> r .! 7
                           <*> r .! 8
                           <*> r .! 9
                           <*> r .! 10
                           <*> r .! 11
                           <*> r .! 12
                           <*> optional (r .! 13)
                           <*> r .! 14
                           <*> optional (r .! 15)
                           <*> r .! 16
                           <*> (isValue "A" <$> r .! 17)
                           <*> r .! 18
                           <*> r .! 19
                           <*> r .! 20
                           <*> r .! 21
                           <*> r .! 22
                           <*> r .! 23
        | otherwise        = fail $ "Invalid PACtoPAC: '" ++ show r ++ "'"
