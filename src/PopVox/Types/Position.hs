{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module PopVox.Types.Position
    ( PositionScore(..)
    , PositionScoreIndex
    , PScoreParty
    ) where


import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T
import           GHC.Generics

import           PopVox.Types.Common


type PositionScoreIndex = M.HashMap (ICPSR, Session) PositionScore
type PScoreParty        = Int

data PositionScore = PScore
                   { psCongress :: !Session
                   , psICPSR    :: !ICPSR
                   , psState    :: !State
                   , psDistrict :: !District
                   , psParty    :: !(Maybe Party)
                   , psName     :: !T.Text
                   , psScore    :: !Score
                   } deriving (Eq, Show, Generic)
