{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Main where


import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List            as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion  hiding (Parser)
import           Data.Either
import           Data.Foldable
import qualified Data.HashMap.Strict          as M
import qualified Data.HashSet                 as S
import qualified Data.List                    as L
import qualified Data.Map.Lazy                as ML
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.Format             as F
import           Filesystem.Path.CurrentOS
import           Prelude                      hiding (FilePath)
import           System.CPUTime

import           Opts
import           PopVox.OpenSecrets
import           PopVox.OpenSecrets.Output
import           PopVox.OpenSecrets.Types
import           PopVox.Types



main :: IO ()
main = do
    PopVoxOptions{..} <- execParser opts

    start <- getCPUTime

    i <- indexFile (_popVoxOpenSecretsDir </> "indivs12.txt")  indexIndiv
    e <- indexFile (_popVoxOpenSecretsDir </> "expends12.txt") indexExp
    let allOrgs = L.sort . S.toList $ orgs i `S.union` orgs e

    runResourceT $  CL.sourceList allOrgs
                 $= CL.map (toMapRow i e)
                 $= (writeHeaders defCSVSettings >> fromCSV defCSVSettings)
                 $$ sinkFile (encodeString _popVoxOutput)

    end <- getCPUTime
    when _popVoxVerbose $ do
        let elapsed = fromIntegral (end - start) / ((10^12) :: Double)
        F.print  "Elapsed: {} sec\n" $ F.Only elapsed
