{-# LANGUAGE OverloadedStrings #-}


module PopVox.MapLight
    ( mapLightUrl
    , billList
    , readContribsC
    ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Aeson
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List            as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion
import qualified Data.Text                    as T
import           Network.Wreq

import           PopVox.Types


mapLightUrl :: String
mapLightUrl = "http://maplight.org/services_open_api"

billList :: String -> ApiKey -> Session -> IO (Either String [OrgInfo])
billList apiUrl apiKey session =
        eitherDecode' . (^. responseBody)
    <$> getWith opts (apiUrl ++ "/map.bill_positions_v1.json")
    where
        opts = defaults & param "apikey"                .~ [apiKey]
                        & param "jurisdiction"          .~ ["us"]
                        & param "session"               .~ [T.pack (show session)]
                        & param "include_organizations" .~ ["1"]
                        & param "has_organizations"     .~ ["0"]

readContribsC :: (Monad m, MonadResource m) => FilePath -> Source m ContribEntry
readContribsC filename =  sourceFile filename
                       $= intoCSV defCSVSettings
                       $= CL.map getNamed
