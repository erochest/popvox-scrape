{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module PopVox.Utils
    ( parseInt
    , log'
    , withLog
    ) where


import           Control.Error
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Buildable
import           Data.Text.Format
import qualified Data.Text.Format          as F
import           Data.Text.Lazy.Builder    as B
import qualified Data.Text.Read            as TR
import           Filesystem.Path.CurrentOS hiding (concat, decode)
import           Prelude                   hiding (FilePath)


parseInt :: Monad m => T.Text -> m Int
parseInt t = case TR.decimal t of
                 Right (n, "") -> return n
                 Right (_, xs) -> fail . T.unpack $ "Extra characters: " <> xs
                 Left e        -> fail e

log' :: Buildable a => F.Format -> a -> IO a
log' f x = F.print f (Only x) >> return x

withLog :: Buildable a => (a -> Script b) -> F.Format -> a -> Script b
withLog m f a = scriptIO (F.print f $ Only a) >> m a

instance Buildable FilePath where
    build = B.fromString . encodeString
