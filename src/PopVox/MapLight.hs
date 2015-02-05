{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}


module PopVox.MapLight
    ( billList
    , indexBills
    , dumpBillIndex
    , dump
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy      as LB
import           Data.Foldable
import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L
import           Filesystem.Path.CurrentOS hiding (encode)
import           Prelude                   hiding (FilePath)

import           PopVox.Types


data BillWrapper = BillWrapper { bills :: [BillInfo] }

instance FromJSON BillWrapper where
    parseJSON (Object o) = BillWrapper <$> o .: "bills"
    parseJSON _          = mzero

billList :: FilePath -> Session -> Script [BillInfo]
billList dataDir session =   hoistEither
                         .   fmap bills
                         .   eitherDecode'
                         =<< scriptIO
                         (   LB.readFile
                         .   encodeString
                         $   dataDir </> decodeString (show session) <.> "json"
                         )

indexBills :: [BillInfo] -> OrgBillIndex
indexBills = foldMap go
    where
        go :: BillInfo -> OrgBillIndex
        go (BillInfo bill orgInfos) = foldMap (go' bill) orgInfos

        go' :: Bill -> OrgInfo -> OrgBillIndex
        go' bill (OrgInfo _ org d) =
            singleHI org $ singleHI bill d

        singleHI k v = HashIndex $ M.singleton k v

dumpBillIndex :: OrgBillIndex -> IO ()
dumpBillIndex = dump "bill-index.json" spreadOrg
    where
        spreadOrg :: (OrgName, BillIndex) -> [(OrgName, (Bill, Disposition))]
        spreadOrg (name, bindex) = map (name,) . M.toList $ unIndex bindex

dump :: ToJSON b => FilePath -> ((k, v) -> [b]) -> HashIndex k v -> IO ()
dump fname f = LB.writeFile (encodeString fname)
             . encode
             . L.concatMap f
             . M.toList
             . unIndex
