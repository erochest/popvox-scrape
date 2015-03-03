{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module PopVox.Action.SearchPosition
    ( searchPosition
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy      as LB
import qualified Data.HashMap.Strict       as M
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Traversable
import           Filesystem.Path.CurrentOS hiding (concat, decode)
import           PopVox.MapLight
import           Prelude                   hiding (FilePath, mapM)

import           PopVox.Types              hiding (maplightAPIDir, maplightOrg)


searchPosition :: FilePath -> T.Text -> Script ()
searchPosition maplightAPIDir maplightOrg = do
    hits <-  fmap M.fromList . forM sessions $ \s ->
             (("session" <>) . T.pack . show $ getSession s,) . filter isHit
         <$> billList maplightAPIDir s
    scriptIO . LB.putStr $ Data.Aeson.encode hits
    where
        isHit :: BillInfo -> Bool
        isHit = any (maplightOrg `T.isInfixOf`) . getOrg

        getOrg bi = bi ^.. billInfoOrgs . traverse . orgInfoName
