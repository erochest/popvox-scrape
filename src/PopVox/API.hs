{-# LANGUAGE OverloadedStrings #-}


module PopVox.API
    ( baseUrl
    , getPages
    , getBills
    , getBillPositions
    ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Text              as T
import           Network.Wreq

import           PopVox.Types


baseUrl :: String
baseUrl = "https://www.popvox.com/"

getPages :: (Functor m, MonadIO m)
         => Options -> String -> PopVoxSessionT m [Response BS.ByteString]
getPages opts url = do
    go (1 :: Int) =<< asks ((opts &) . set (param "api_key") . pure . getAPIKey)
    where
        go pg o = do
            let o' = o & param "page" .~ [T.pack $ show pg]
            resp <- liftIO $ getWith o' url
            if (resp ^? responseBody . key "has_next" . _Bool == Just True)
                then (resp :) <$> go (succ pg) o
                else return [resp]

getBills :: MonadIO m => PopVoxSessionT m [Bill]
getBills = undefined

getBillPositions :: MonadIO m => BillID -> PopVoxSessionT m [OrgPosition]
getBillPositions = undefined
