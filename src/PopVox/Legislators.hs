{-# LANGUAGE OverloadedStrings #-}


module PopVox.Legislators
    ( readLegislatorIndex
    ) where


import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict       as M
import           Data.Yaml
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

import           PopVox.Types


-- | Maps from thomas to icpsr IDs. Doesn't contain any more information
-- about the reps.
readLegislatorIndex :: FilePath -> Script LegislatorIndex
readLegislatorIndex = EitherT
                    . fmap (show `bimap` legislator)
                    . decodeFileEither
                    . encodeString

legislator :: Value -> LegislatorIndex
legislator v = v ^.. _Array . traverse . key "id"
             & mapMaybe ( uncurry (liftA2 (,))
                        . (strkey "thomas" &&& intkey "icpsr"))
             & map (Thomas `bimap` ICPSR)
             & M.fromList
    where
        strkey k = preview (key k . _String)
        intkey k = preview (key k . _Integer . to fromIntegral)
