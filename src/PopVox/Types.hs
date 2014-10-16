{-# LANGUAGE TemplateHaskell #-}


module PopVox.Types
    ( PopVoxOptions(..)
    , popVoxOpenSecretsDir
    , popVoxVerbose
    ) where


import           Control.Lens
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)


data PopVoxOptions = PopVoxOptions
    { _popVoxOpenSecretsDir :: !FilePath
    , _popVoxOutput         :: !FilePath
    , _popVoxVerbose        :: !Bool
    } deriving (Show)
makeLenses ''PopVoxOptions
