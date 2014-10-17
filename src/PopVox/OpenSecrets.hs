{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module PopVox.OpenSecrets
    ( csvSettings
    , readOpenSecrets
    , readOpenSecretsC
    , parseOpenSecrets
    , indexFile
    , indexFileP
    , chunks
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Par
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as C8
import           Data.Conduit
import qualified Data.Conduit.Binary             as CB
import qualified Data.Conduit.List               as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import           Data.Foldable
import qualified Data.List                       as L
import           Data.Monoid
import           Data.Traversable                hiding (mapM)
import qualified Data.Vector                     as V
import           Filesystem.Path.CurrentOS
import           Prelude                         hiding (FilePath)
import           System.IO                       hiding (FilePath)

import           PopVox.OpenSecrets.Types.Common


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
    CB.sourceFile (encodeString filePath) $= intoCSV csvSettings

parseOpenSecrets :: (Monad m, FromRecord a)
                 => Conduit Record m (Either String a)
parseOpenSecrets = CL.map (runParser . parseRecord)

indexFile :: (FromRecord a, Num b)
          => FilePath -> (Either String a -> OrgIndex b) -> IO (OrgIndex b)
indexFile input f =  runResourceT
                  $  readOpenSecretsC input
                  $= parseOpenSecrets
                  $$ CL.foldMap f

chunks :: Monad m => Int -> Conduit a m [a]
chunks size = go
    where
        go = do
            c <- CL.take size
            when (not (L.null c)) $
                yield c
            when (length c == size)
                go

indexFileP :: (FromRecord a, Num b, NFData b)
           => FilePath
           -> Int
           -> Int
           -> (Either String a -> OrgIndex b)
           -> IO (OrgIndex b)
indexFileP input chunkBytes foldChunkSize f =
    withFile (encodeString input) ReadMode $ \h -> do
        cs <- liftIO (readChunks h chunkBytes)
        runParIO $ do
            vars <- mapM (spawn . return . processChunk f) cs
            foldAll foldChunkSize vars
    where
        foldAll :: (NFData a, Monoid a) => Int -> [IVar a] -> Par a
        foldAll _ [v]   = get v
        foldAll size vs =   foldAll size
                        =<< mapM (spawn . foldM getFold mempty)
                                 (partition size vs)

        getFold :: Monoid a => a -> IVar a -> Par a
        getFold a v = mappend a <$> get v

partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n xs = uncurry (:) . fmap (partition n) $ splitAt n xs

processChunk :: (FromRecord a, Num b)
             => (Either String a -> OrgIndex b)
             -> BS.ByteString
             -> OrgIndex b
processChunk f input =
      foldMap (f . Right)
    . fold
    . join
    . bimap show (traverse (runParser . parseRecord))
    $ (decodeCSV csvSettings input :: Either SomeException (V.Vector Record))

readChunks :: Handle -> Int -> IO [C8.ByteString]
readChunks h size = do
    rawChunk <- C8.hGet h size
    if C8.null rawChunk
        then return []
        else getRest rawChunk
    where
        getRest current = handleRest current <$> readChunks h size

        handleRest c []         = [c]
        handleRest c rrs@(r:rs) =
            maybe (c:rrs) (shiftLast r rs) . initLast $ C8.lines c

        shiftLast next nexts (lnes, lne) =
            (C8.unlines lnes) : (lne <> next) : nexts

initLast :: [a] -> Maybe ([a], a)
initLast []     = Nothing
initLast [x]    = Just ([], x)
initLast (x:xs) = fmap (first (x:)) (initLast xs)

    {-
     -     runResourceT
     - $  CB.sourceFile (encodeString input)
     - $= CT.decode CT.iso8859_1
     - $= CT.lines
     - $= chunks 4096
     - -- start parallel here
     - $= CL.map T.unlines
     - $= intoCSV csvSettings
     - $= parseOpenSecrets
     - $$ _sink
     -}
    {-
     - CB.sourceFile (encodeString input)
     -     $= CT.encode CT.iso8859_1
     -     $= CT.lines
     -     $= chunks 4096
     -}
