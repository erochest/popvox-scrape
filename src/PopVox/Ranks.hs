{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module PopVox.Ranks
    ( resolvePartyCode
    , resolveStateCode
    , readPositionScores
    , indexPositionScores
    , assembleRanks
    ) where


import Data.Monoid
import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Data.Foldable
import qualified Data.HashMap.Strict       as M
import           Data.Maybe
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Data.Traversable
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)

import           PopVox.Fields
import           PopVox.Types


resolvePartyCode :: PScoreParty -> Maybe Party
resolvePartyCode 100 = Just Dem
resolvePartyCode 200 = Just GOP
resolvePartyCode _   = Nothing

resolveStateCode :: StateCode -> Maybe State
resolveStateCode 41 = Just "AL"
resolveStateCode 81 = Just "AK"
resolveStateCode 61 = Just "AZ"
resolveStateCode 42 = Just "AR"
resolveStateCode 71 = Just "CA"
resolveStateCode 62 = Just "CO"
resolveStateCode  1 = Just "CT"
resolveStateCode 11 = Just "DE"
resolveStateCode 43 = Just "FL"
resolveStateCode 44 = Just "GA"
resolveStateCode 82 = Just "HI"
resolveStateCode 63 = Just "ID"
resolveStateCode 21 = Just "IL"
resolveStateCode 22 = Just "IN"
resolveStateCode 31 = Just "IA"
resolveStateCode 32 = Just "KS"
resolveStateCode 51 = Just "KY"
resolveStateCode 45 = Just "LA"
resolveStateCode  2 = Just "ME"
resolveStateCode 52 = Just "MD"
resolveStateCode  3 = Just "MA"
resolveStateCode 23 = Just "MI"
resolveStateCode 33 = Just "MN"
resolveStateCode 46 = Just "MS"
resolveStateCode 34 = Just "MO"
resolveStateCode 64 = Just "MT"
resolveStateCode 35 = Just "NE"
resolveStateCode 65 = Just "NV"
resolveStateCode 04 = Just "NH"
resolveStateCode 12 = Just "NJ"
resolveStateCode 66 = Just "NM"
resolveStateCode 13 = Just "NY"
resolveStateCode 47 = Just "NC"
resolveStateCode 36 = Just "ND"
resolveStateCode 24 = Just "OH"
resolveStateCode 53 = Just "OK"
resolveStateCode 72 = Just "OR"
resolveStateCode 14 = Just "PA"
resolveStateCode  5 = Just "RI"
resolveStateCode 48 = Just "SC"
resolveStateCode 37 = Just "SD"
resolveStateCode 54 = Just "TN"
resolveStateCode 49 = Just "TX"
resolveStateCode 67 = Just "UT"
resolveStateCode  6 = Just "VT"
resolveStateCode 40 = Just "VA"
resolveStateCode 73 = Just "WA"
resolveStateCode 56 = Just "WV"
resolveStateCode 25 = Just "WI"
resolveStateCode 68 = Just "WY"
resolveStateCode 55 = Just "DC"
resolveStateCode 99 = Just "USA"
resolveStateCode _  = Nothing

readPositionScores :: FilePath -> Script [PositionScore]
readPositionScores input =
    EitherT $   sequenceA . map parsePosition . T.lines
            <$> TIO.readFile (encodeString input)

parsePosition :: T.Text -> Either String PositionScore
parsePosition =
    runFields $ PScore <$> fmap Session (asint =<< field 0 4)
                       <*> fmap ICPSR (asint =<< field 5 5)
                       <*> pnote "Invalid state." ( fmap resolveStateCode
                                                  . asint
                                                  =<< field 11 2)
                       <*> (asint =<< field 13 2)
                       <*> fmap resolvePartyCode (asint =<< field 25 4)
                       <*> fmap T.strip (field 29 11)
                       <*> (asdouble =<< field 41 6)

indexPositionScores :: [PositionScore] -> PositionScoreIndex
indexPositionScores = M.fromList . map ((psICPSR &&& psCongress) &&& id)

assembleRanks :: PositionScoreIndex -> [BillSponsors (LegislatorInfo ICPSR)]
              -> [BillRankData]
assembleRanks psIndex =
    map (toData . (billSponsorInfo &&& getScore psIndex &&& getPartyCounts))
    where
        toData (bill, ((count, score), (d, r))) = BillRankData bill count d r score

getPartyCounts :: BillSponsors (LegislatorInfo a) -> (Int, Int)
getPartyCounts s = (getSum *** getSum) . foldMap (countParty . legParty)
                 $ maybeToList (billSponsor s) ++ billCosponsors s
    where
        countParty Dem = (Sum 1, Sum 0)
        countParty GOP = (Sum 0, Sum 1)
        countParty _   = (Sum 0, Sum 0)

getScore :: PositionScoreIndex -> BillSponsors (LegislatorInfo ICPSR)
         -> (Int, Score)
getScore psIndex bill@BillSponsors{billSponsorInfo} =
      mean
    . map psScore
    . mapMaybe ((`M.lookup` psIndex) . (, billSession billSponsorInfo) . legID)
    $ getAllSponsors bill

getAllSponsors :: BillSponsors s -> [s]
getAllSponsors (BillSponsors _ s ss) = maybeToList s ++ ss

mean :: [Score] -> (Int, Score)
mean [] = (0, 0.0)
mean ss = fini $ foldl' go (0, 0 :: Int) ss
    where
        go (s', c) s = (s' + s, succ c)
        fini (s, c)  = (c, s / fromIntegral c)
