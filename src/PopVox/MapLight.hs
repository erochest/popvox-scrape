{-# LANGUAGE OverloadedStrings #-}


module PopVox.MapLight
    ( mapLightUrl
    , billList
    , readContribsC
    , indexContribsC
    , readIndexContribs
    , indexBills
    , toData
    ) where


import           Control.Applicative
import           Control.Lens                 hiding ((<.>))
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Aeson
import qualified Data.ByteString.Lazy         as B
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List            as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import           Data.Foldable
import qualified Data.HashMap.Strict          as M
import qualified Data.List                    as L
import           Data.Monoid
import           Data.Ord
import qualified Data.Text                    as T
import           Filesystem
import           Filesystem.Path.CurrentOS
import           Network.Wreq
import           Prelude                      hiding (FilePath)

import           PopVox.Types


mapLightUrl :: String
mapLightUrl = "http://maplight.org/services_open_api"

billList :: FilePath -> String -> ApiKey -> Session
         -> IO (Either String [BillInfo])
billList cacheDir apiUrl apiKey session = do
    exists <- isFile cacheFile
    body   <- if exists
                  then B.readFile $ encodeString cacheFile
                  else download
    return $ eitherDecode' body

    where
        cacheFile = cacheDir </> decodeString (show session) <.> "json"
        download = do
            body <-  (^. responseBody)
                 <$> getWith opts (apiUrl ++ "/map.bill_positions_v1.json")
            B.writeFile (encodeString cacheFile) body
            return body
        opts = defaults & param "apikey"                .~ [apiKey]
                        & param "jurisdiction"          .~ ["us"]
                        & param "session"               .~ [T.pack (show session)]
                        & param "include_organizations" .~ ["1"]
                        & param "has_organizations"     .~ ["0"]

indexBills :: [BillInfo] -> OrgBillIndex
indexBills = HashIndex . foldMap go
    where
        go (BillInfo bill orgInfos) = foldMap (go' bill) orgInfos
        go' bill (OrgInfo org d) = M.singleton org $ M.singleton bill d

readContribsC :: (Monad m, MonadResource m)
              => FilePath -> Source m OrgContrib
readContribsC input =  sourceFile (encodeString input)
                    $= intoCSV defCSVSettings
                    $= CL.map getNamed

indexContribsC :: Monad m => Sink OrgContrib m OrgContribIndex
indexContribsC = CL.foldMap $ \(OrgContrib name entry) ->
    HashIndex . M.singleton name . HashIndex . M.singleton entry $ Sum 1

readIndexContribs :: FilePath -> IO OrgContribIndex
readIndexContribs input = runResourceT $ readContribsC input $$ indexContribsC

toData :: OrgBillIndex -> OrgContribIndex -> [OrgData]
toData (HashIndex billIndex) (HashIndex contribIndex) =
    L.sortBy (comparing orgName) . map toOrgData $ M.toList billIndex
    where
        toOrgData :: (OrgName, BillIndex) -> OrgData
        toOrgData (n, b) = Org n (fold $ M.lookup n contribIndex) b
