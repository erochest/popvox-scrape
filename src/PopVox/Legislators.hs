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


-- | Maps from icpsr to govtrack IDs. Doesn't contain any more information
-- about the reps.
readLegislatorIndex :: FilePath -> Script LegislatorIndex
readLegislatorIndex = EitherT
                    . fmap (show `bimap` legislator)
                    . decodeFileEither
                    . encodeString

legislator :: Value -> LegislatorIndex
legislator v = v ^.. _Array . traverse . key "id"
             & mapMaybe ( uncurry (liftA2 (,))
                        . (intkey "icpsr" &&& intkey "govtrack"))
             & M.fromList
    where
        intkey k = (^? key k . _Integer . to fromIntegral)
