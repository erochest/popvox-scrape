{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module PopVox.OpenSecrets
    ( csvSettings
    , readOpenSecrets
    , readOpenSecretsC
    ) where


import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.CSV.Conduit
import qualified Data.Vector                  as V
import           Filesystem.Path.CurrentOS
import           Prelude                      hiding (FilePath)


csvSettings :: CSVSettings
csvSettings = CSVSettings ',' $ Just '|'

readOpenSecrets :: (MonadIO m, CSV BS.ByteString a) => FilePath -> m (V.Vector a)
readOpenSecrets = readCSVFile csvSettings . encodeString

readOpenSecretsC :: (MonadResource m, CSV BS.ByteString a)
                 => FilePath -> Source m a
readOpenSecretsC filePath =
    sourceFile (encodeString filePath) $= intoCSV csvSettings
