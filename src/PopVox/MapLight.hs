{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}


module PopVox.MapLight
    ( billList
    , readContribsC
    , indexContribsC
    , readIndexContribs
    , indexBills
    , toData
    , dumpBillIndex
    , dumpContribIndex
    ) where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Aeson
import qualified Data.ByteString.Lazy         as B
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List            as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion  hiding ((.:))
import           Data.Foldable
import qualified Data.HashMap.Strict          as M
import qualified Data.List                    as L
import           Data.Monoid
import           Data.Ord
import           Filesystem.Path.CurrentOS    hiding (encode)
import           Prelude                      hiding (FilePath)

import           PopVox.Types


data BillWrapper = BillWrapper { bills :: [BillInfo] }

instance FromJSON BillWrapper where
    parseJSON (Object o) = BillWrapper <$> o .: "bills"
    parseJSON _          = mzero

billList :: FilePath -> Session -> IO (Either String [BillInfo])
billList dataDir session =
    fmap bills . eitherDecode' <$> B.readFile
                               (   encodeString
                               $   dataDir </> decodeString (show session) <.> "json")

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

dumpBillIndex :: OrgBillIndex -> IO ()
dumpBillIndex = dump "bill-index.json" spreadOrg
    where
        spreadOrg (name, bindex) = map (name,) $ M.toList bindex

dumpContribIndex :: OrgContribIndex -> IO ()
dumpContribIndex = dump "contrib-index.json" spreadOrg
    where
        spreadOrg (name, cindex) = map (name,)
                                 . M.toList
                                 . fmap getSum
                                 $ unIndex cindex

dump :: ToJSON b => FilePath -> ((k, v) -> [b]) -> HashIndex k v -> IO ()
dump fname f = B.writeFile (encodeString fname)
             . encode
             . L.concatMap f
             . M.toList
             . unIndex
