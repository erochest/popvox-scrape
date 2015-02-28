{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module PopVox.Contribs
    ( majorParty
    , readContribs
    , indexContribs
    , indexContribs'
    , readIndexContribs
    , toData
    , dumpContribIndex
    ) where


import           Conduit
import           Control.Error
import           Data.Bifunctor
import qualified Data.ByteString.Lazy      as LB
import           Data.Csv                  hiding (encode, (.:))
import           Data.Csv.Conduit
import           Data.Foldable
import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L
import           Data.Monoid
import           Data.Ord
import qualified Data.Text                 as T
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
        f (OrgContrib name district entry amount) =
          HashIndex . M.singleton name
                    . HashIndex
                    $ M.singleton entry (First (justText district), Sum amount)

indexContribs' :: (Header, V.Vector OrgContrib) -> OrgContribIndex
indexContribs' = indexContribs
               . V.filter (majorParty . contribParty . orgContribEntry)
               . snd

readIndexContribs :: FilePath -> Script OrgContribIndex
readIndexContribs fn =  runResourceT
                     $  sourceFile fn
                     $= fromNamedCsvLiftError toErr defaultDecodeOptions
                     $= filterC keepContrib
                     $$ foldMapC idx
    where
        keepContrib (OrgContrib _ _ (Candidate _ Dem) _) = True
        keepContrib (OrgContrib _ _ (Candidate _ GOP) _) = True
        keepContrib _                                  = False

        idx (OrgContrib name district entry amount) =
            HashIndex . M.singleton name
                      . HashIndex
                      $ M.singleton entry (First (justText district), Sum amount)

        toErr (CsvParseError input er) =  "PARSE ERROR: " ++ er
                                       ++ ": <<<" ++ show input ++ ">>>"
        toErr (IncrementalError er)    =  "INCREMENTAL ERROR: " ++ er

justText :: T.Text -> Maybe T.Text
justText t | T.null t  = Nothing
           | otherwise = Just t

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
                                 . fmap (getFirst `bimap` getSum)
                                 $ unIndex cindex
