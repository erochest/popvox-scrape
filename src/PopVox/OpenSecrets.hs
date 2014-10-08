{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module PopVox.OpenSecrets
    ( csvSettings
    , readOpenSecrets
    , readOpenSecretsC
    , parseOpenSecrets
    ) where


import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List            as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import           Data.Traversable
import qualified Data.Vector                  as V
import           Filesystem.Path.CurrentOS
import           Prelude                      hiding (FilePath)

import           PopVox.OpenSecrets.Types


csvSettings :: CSVSettings
csvSettings = CSVSettings ',' $ Just '|'

readOpenSecrets :: (Functor m, MonadIO m, FromRecord a)
                => FilePath -> m (Either String (V.Vector a))
readOpenSecrets = fmap (traverse (runParser . parseRecord))
                . readCSVFile csvSettings
                . encodeString

readOpenSecretsC :: (MonadResource m, CSV BS.ByteString a)
                 => FilePath -> Source m a
readOpenSecretsC filePath =
    sourceFile (encodeString filePath) $= intoCSV csvSettings

parseOpenSecrets :: (Monad m, FromRecord a)
                 => Conduit Record m (Either String a)
parseOpenSecrets = CL.map (runParser . parseRecord)
