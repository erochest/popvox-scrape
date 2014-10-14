{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Lens                 hiding (argument)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import           Data.Conduit
import qualified Data.Conduit.List            as CL
import           Data.CSV.Conduit.Conversion  hiding (Parser)
import           Data.Either
import           Data.Either
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Format             as F
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
    (PopVoxOptions{..}, query) <- execParser opts

    start <- getCPUTime

    F.print "Querying \"{}\"\n" $ F.Only query
    count <- runResourceT $ readOpenSecretsC (_popVoxOpenSecretsDir </> "indivs12.txt")
                 $= parseOpenSecrets
                 $= CL.filter ((== (Just query)) . preview (_Right . indOrgName))
                 -- $$ CL.consume
                 $$ CL.foldMapM (const (return (Sum 1)) <=< liftIO . print)

    end <- getCPUTime
    when _popVoxVerbose $ do
        let elapsed = (fromIntegral (end - start)) / ((10^12) :: Double)
        F.print "Results: {}\n"     . F.Only $ getSum (count :: Sum Int)
        F.print "Elapsed: {} sec\n" $ F.Only elapsed

testOpenSecrets :: (FromRecord a, Show a)
                => FilePath -> Translator a -> IO ()
testOpenSecrets filepath translator = do
    print filepath
    start <- getCPUTime
    (errs, oks) <- fmap (bimap getSum getSum) $ runResourceT $
        readOpenSecretsC filepath $= translator $$ CL.foldMapM accum
    end <- getCPUTime
    let elapsed = ((fromIntegral (end - start)) / ((10^(12 :: Int)) :: Double)) :: Double
    putStrLn $ "ERROR COUNT: " ++ show (errs :: Int)
    putStrLn $ "OK    COUNT: " ++ show (oks :: Int)
    printf     "Elapsed    : %0.3f sec\n" (elapsed :: Double)
    putStrLn ""

    where
        accum (Left err) =  liftIO (putStrLn $ "ERROR: " ++ err)
                         >> return (Sum 1, mempty)
        accum (Right _)  = return (mempty, Sum 1)

opts' :: Parser (PopVoxOptions, T.Text)
opts' = (,) <$> popVox <*> query
    where
        popVox =   PopVoxOptions
               <$> fileOpt (  short 's'
                           <> long "opensecrets"
                           <> metavar "DIRNAME"
                           <> help "The location of the opensecrets.org data sets.")
               <*> switch  (  short 'v'
                           <> long "verbose"
                           <> help "Print extra information.")
        query  = argument (Just . T.pack) (  metavar "QUERY"
                                          <> help "The organization to query for.")

opts :: ParserInfo (PopVoxOptions, T.Text)
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Scrapes data from popvox.com and \
                        \mashes it with opensecrets.org data."
            <> header ("popvox-scrape v" ++ showVersion version))

fileOpt :: Mod OptionFields FilePath -> Parser FilePath
fileOpt = option (pure . decodeString)
