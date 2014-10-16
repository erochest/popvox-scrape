{-# LANGUAGE OverloadedStrings #-}


module PopVox.OpenSecrets.Output
    ( toMapRow
    ) where


import qualified Data.ByteString             as BS
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion hiding (Parser)
import           Data.Foldable               (fold)
import qualified Data.HashMap.Strict         as M
import qualified Data.Map.Lazy               as ML
import           Data.Monoid
import qualified Data.Text                   as T
import           Data.Text.Encoding

import           PopVox.OpenSecrets.Types
import           PopVox.Types


toMapRow :: OrgIndex Int -> OrgIndex Double -> T.Text -> MapRow BS.ByteString
toMapRow indivs expends org =
    ML.fromList [ ("Organization", encodeUtf8 org)
                , ("Dem12",    toField $ lu indivs  org Dem)
                , ("GOP12",    toField $ lu indivs  org Rep)
                , ("DemInd12", toField $ lu expends org Dem)
                , ("GOPInd12", toField $ lu expends org Rep)
                ]
    where
        lu :: Num a => OrgIndex a -> T.Text -> Party -> a
        lu m o p =   getSum . fold
                 $   M.lookup p . getIndex
                 =<< M.lookup o (getIndex m)

