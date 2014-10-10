{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import           Data.CSV.Conduit.Conversion  hiding (Parser)
import           Data.Either
import           Data.Monoid
import           Data.Version
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                      hiding (FilePath)
import           System.CPUTime
import           Text.Printf

import           Paths_popvox_scrape
import           PopVox.OpenSecrets
import           PopVox.OpenSecrets.Types
import           PopVox.Types


type Translator a = Conduit Record (ResourceT IO) (Either String a)


main :: IO ()
main = do
    PopVoxOptions{..} <- execParser opts

    testOpenSecrets (_popVoxOpenSecretsDir </> "pac_other12.txt")
                    (parseOpenSecrets :: Translator PACtoPAC)
    testOpenSecrets (_popVoxOpenSecretsDir </> "cmtes12.txt")
                    (parseOpenSecrets :: Translator CommitteeRecord)
    testOpenSecrets (_popVoxOpenSecretsDir </> "cands12.txt")
                    (parseOpenSecrets :: Translator Candidate)
    testOpenSecrets (_popVoxOpenSecretsDir </> "indivs12.txt")
                    (parseOpenSecrets :: Translator Individual)
    testOpenSecrets (_popVoxOpenSecretsDir </> "pacs12.txt")
                    (parseOpenSecrets :: Translator PACCandidate)

testOpenSecrets :: (FromRecord a, Show a)
                => FilePath -> Translator a -> IO ()
testOpenSecrets filepath translator = do
    print filepath
    start <- getCPUTime
    (errs, oks) <- fmap (bimap getSum getSum) $ runResourceT $
        readOpenSecretsC filepath $= translator $$ CL.foldMapM accum
    end <- getCPUTime
    let elapsed = (fromIntegral (end - start)) / ((10^12) :: Double)
    putStrLn $ "ERROR COUNT: " ++ show (errs :: Int)
    putStrLn $ "OK    COUNT: " ++ show (oks :: Int)
    printf     "Elapsed    : %0.3f sec\n" (elapsed :: Double)
    putStrLn ""

    where
        accum (Left err) =  liftIO (putStrLn $ "ERROR: " ++ err)
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
