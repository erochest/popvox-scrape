{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module PopVox.Contribs
    ( majorParty
    , readContribs
    , indexContribs
    , indexContribs'
    , readIndexContribs
    , toData
    , dumpContribIndex
    ) where


import           Control.Applicative
import           Control.Error
import qualified Data.ByteString.Lazy      as LB
import           Data.Csv                  hiding (encode, (.:))
import           Data.Foldable
import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L
import           Data.Monoid
import           Data.Ord
import qualified Data.Vector               as V
import           Filesystem.Path.CurrentOS hiding (encode)
import           Prelude                   hiding (FilePath)

import           PopVox.MapLight
import           PopVox.Types


majorParty :: Party -> Bool
majorParty Dem = True
majorParty GOP = True
majorParty _   = False

readContribs :: FilePath -> Script (Header, V.Vector OrgContrib)
readContribs fn =
    hoistEither . decodeByName =<< scriptIO (LB.readFile $ encodeString fn)

indexContribs :: V.Vector OrgContrib -> OrgContribIndex
indexContribs = foldMap f
    where
        f (OrgContrib name entry amount) =
          HashIndex . M.singleton name . HashIndex . M.singleton entry $ Sum amount

indexContribs' :: (Header, V.Vector OrgContrib) -> OrgContribIndex
indexContribs' = indexContribs
               . V.filter (majorParty . contribParty . orgContribEntry)
               . snd

readIndexContribs :: FilePath -> Script OrgContribIndex
readIndexContribs input = indexContribs . snd <$> readContribs input

toData :: OrgBillIndex -> OrgContribIndex -> [OrgData]
toData (HashIndex billIndex) (HashIndex contribIndex) =
    L.sortBy (comparing orgName) . map toOrgData $ M.toList billIndex
    where
        toOrgData :: (OrgName, BillIndex) -> OrgData
        toOrgData (n, b) = Org n (fold $ M.lookup n contribIndex) b

dumpContribIndex :: OrgContribIndex -> IO ()
dumpContribIndex = dump "contrib-index.json" spreadOrg
    where
        spreadOrg (name, cindex) = map (name,)
                                 . M.toList
                                 . fmap getSum
                                 $ unIndex cindex
