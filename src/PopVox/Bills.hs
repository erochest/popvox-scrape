{-# LANGUAGE OverloadedStrings #-}


module PopVox.Bills
    ( readBillData
    , readBillDataDir
    , resolveIDs
    ) where


import           Conduit
import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy      as LB
import qualified Data.HashMap.Strict       as M
import           Data.Traversable
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

import           PopVox.Types


readBillData :: FilePath -> IO (Either String (BillSponsors Thomas))
readBillData input =
    eitherDecode' . LB.fromStrict <$> FS.readFile input

readBillDataDir :: FilePath -> Script [BillSponsors Thomas]
readBillDataDir dataDir = EitherT . fmap sequenceA . runResourceT $
       sourceDirectoryDeep True dataDir
    =$ filterC (("data.json" ==) . filename)
    =$ mapMC (liftIO . readBillData)
    $$ sinkList

resolveIDs :: LegislatorIndex -> BillSponsors Thomas -> BillSponsors ICPSR
resolveIDs index = joinm . fmap (`M.lookup` index)
    where
        joinm (BillSponsors b s ss) = BillSponsors b (join s) $ catMaybes ss
