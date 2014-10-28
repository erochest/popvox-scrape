{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module PopVox.OpenSecrets.Output
    ( individualReport
    , committeeReport
    ) where


import qualified Data.ByteString             as BS
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List           as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion hiding (Parser)
import           Data.Foldable               (fold)
import qualified Data.HashMap.Strict         as M
import qualified Data.HashSet                as S
import qualified Data.List                   as L
import qualified Data.Map.Lazy               as ML
import           Data.Monoid
import qualified Data.Text                   as T
import           Data.Text.Encoding
import           Filesystem.Path.CurrentOS
import           Prelude                     hiding (FilePath)

import           PopVox.OpenSecrets
import           PopVox.OpenSecrets.Types
import           PopVox.Types


individualReport :: PopVoxOptions -> IO ()
individualReport PopVoxOptions{..} = do
    i <- indexFile (_popVoxOpenSecretsDir </> "indivs12.txt")  indexIndiv
    e <- indexFile (_popVoxOpenSecretsDir </> "expends12.txt") indexExp
    let allOrgs = L.sort . S.toList $ orgs i `S.union` orgs e
    writeReport allOrgs (indivRow i e) _popVoxOutput

committeeReport :: PopVoxOptions -> IO ()
committeeReport PopVoxOptions{..} = do
    c <-  indexFile (_popVoxOpenSecretsDir </> "cmtes12.txt")   . indexCmte
      =<< indexFile (_popVoxOpenSecretsDir </> "indivs12.txt")  indexByCmte
    e <-  indexFile (_popVoxOpenSecretsDir </> "expends12.txt") indexExp
    let allOrgs = L.sort . M.keys $ getIndex c
    writeReport allOrgs (indivRow c e) _popVoxOutput

writeReport :: [T.Text] -> (T.Text -> Maybe (MapRow BS.ByteString)) -> FilePath
            -> IO ()
writeReport keys toMapRow output =
    runResourceT $  CL.sourceList keys
                 $= CL.mapMaybe toMapRow
                 $= (writeHeaders defCSVSettings >> fromCSV defCSVSettings)
                 $$ sinkFile (encodeString output)

indivRow :: OrgIndex Int -> OrgIndex Double -> T.Text
         -> Maybe (MapRow BS.ByteString)
indivRow indivs expends org
    | L.any ((> 0) . snd) amounts = Just . ML.fromList
                                         $ ("Organization", encodeUtf8 org)
                                         : map (fmap toField) amounts
    | otherwise = Nothing
    where
        amounts = [ ("Dem12",    lu indivs org Dem)
                  , ("GOP12",    lu indivs org Rep)
                  , ("DemInd12", truncate $ lu expends org Dem)
                  , ("GOPInd12", truncate $ lu expends org Rep)
                  ]
        lu :: Num a => OrgIndex a -> T.Text -> Party -> a
        lu m o p =   getSum . fold
                 $   M.lookup p . getIndex
                 =<< M.lookup o (getIndex m)
