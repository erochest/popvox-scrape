{-# LANGUAGE TemplateHaskell #-}


module PopVox.Types
    ( PopVoxOptions(..)
    , popVoxOpenSecretsDir
    , popVoxOutput
    , popVoxVerbose

    , HashIndex(..)
    ) where


import           Control.Lens
import qualified Data.HashMap.Strict       as M
import           Data.Monoid
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)
import           Data.Hashable


data PopVoxOptions = PopVoxOptions
    { _popVoxOpenSecretsDir :: !FilePath
    , _popVoxOutput         :: !FilePath
    , _popVoxVerbose        :: !Bool
    } deriving (Show)
makeLenses ''PopVoxOptions

newtype HashIndex k v = HashIndex { getIndex :: M.HashMap k v }

instance (Eq k, Hashable k, Monoid v) => Monoid (HashIndex k v) where
    mempty = HashIndex mempty
    mappend (HashIndex m1) (HashIndex m2) = HashIndex $ M.unionWith mappend m1 m2
