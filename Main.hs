{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import qualified Data.ByteString              as B
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion  hiding (Parser)
import           Data.Either
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import           Data.Version
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                      hiding (FilePath)

import           Paths_popvox_scrape
import           PopVox.OpenSecrets
import           PopVox.OpenSecrets.Types
import           PopVox.Types


type Translator a = Conduit Record (ResourceT IO) (Either String a)


main :: IO ()
main = do
    PopVoxOptions{..} <- execParser opts
    testOpenSecrets (_popVoxOpenSecretsDir </> "cmtes12.txt")
                    (parseOpenSecrets :: Translator CommitteeRecord)
    testOpenSecrets (_popVoxOpenSecretsDir </> "cands12.txt")
                    (parseOpenSecrets :: Translator Candidate)

testOpenSecrets :: (FromRecord a, Show a)
                => FilePath -> Translator a -> IO ()
testOpenSecrets filepath translator = do
    print filepath
    (errs, oks) <- fmap (bimap getSum getSum) $ runResourceT $
        readOpenSecretsC filepath $= translator $$ CL.foldMapM accum
    putStrLn $ "ERROR COUNT: " ++ show errs
    putStrLn $ "OK    COUNT: " ++ show oks
    putStrLn ""

    where
        accum (Left err) =  liftIO (print $ "ERROR: " ++ err)
                         >> return (Sum 1, mempty)
        accum (Right _)  = return (mempty, Sum 1)

opts' :: Parser PopVoxOptions
opts' =   PopVoxOptions
      <$> fileOpt (  short 's'
                  <> long "opensecrets"
                  <> metavar "DIRNAME"
                  <> help "The location of the opensecrets.org data sets.")

opts :: ParserInfo PopVoxOptions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Scrapes data from popvox.com and \
                        \mashes it with opensecrets.org data."
            <> header ("popvox-scrape v" ++ showVersion version))

fileOpt :: Mod OptionFields FilePath -> Parser FilePath
fileOpt = option (pure . decodeString)
