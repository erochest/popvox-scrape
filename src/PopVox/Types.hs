{-# LANGUAGE TemplateHaskell #-}


module PopVox.Types
    ( PopVoxOptions(..)
    , popVoxOpenSecretsDir
    ) where


import           Control.Lens
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)


data PopVoxOptions = PopVoxOptions
    { _popVoxOpenSecretsDir :: FilePath
    } deriving (Show)
makeLenses ''PopVoxOptions
