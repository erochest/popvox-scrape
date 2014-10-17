{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}


module PopVox.Types
    ( PopVoxOptions(..)
    , popVoxOpenSecretsDir
    , popVoxOutput
    , popVoxVerbose

    , HashIndex(..)
    ) where


import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import           Data.Monoid
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)


data PopVoxOptions = PopVoxOptions
    { _popVoxOpenSecretsDir :: !FilePath
    , _popVoxOutput         :: !FilePath
    , _popVoxVerbose        :: !Bool
    } deriving (Show)
makeLenses ''PopVoxOptions

newtype HashIndex k v = HashIndex { getIndex :: M.HashMap k v }
                        deriving (NFData)

instance Functor (HashIndex k) where
    fmap f = HashIndex . fmap f . getIndex

instance (Eq k, Hashable k, Monoid v) => Monoid (HashIndex k v) where
    mempty = HashIndex mempty
    mappend (HashIndex m1) (HashIndex m2) = HashIndex $ M.unionWith mappend m1 m2

instance Foldable (HashIndex k) where
    foldMap f m = foldMap f $ getIndex m

instance Traversable (HashIndex k) where
    traverse f m = fmap HashIndex <$> traverse f $ getIndex m
