{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Monad.IO.Class
import qualified Data.ByteString           as B
import           Data.Conduit
import qualified Data.Conduit.List         as CL
import           Data.CSV.Conduit
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Data.Version
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                   hiding (FilePath)

import           Paths_popvox_scrape
import           PopVox.OpenSecrets
import           PopVox.OpenSecrets.Types
import           PopVox.Types


main :: IO ()
main = do
    PopVoxOptions{..} <- execParser opts
    let filename = _popVoxOpenSecretsDir </> "cmtes12.txt"
    runResourceT $ readOpenSecretsC filename
        $= parseOpenSecrets
        $$ CL.mapM_ (liftIO . print)


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
