{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Main where


import Data.Foldable
import           Control.Lens                 hiding (argument)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import qualified Data.ByteString              as BS
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List            as CL
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion  hiding (Parser)
import           Data.Either
import           Data.Hashable
import qualified Data.HashMap.Strict          as M
import qualified Data.HashSet                 as S
import qualified Data.List                    as L
import qualified Data.Map.Lazy                as ML
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Encoding
import qualified Data.Text.Format             as F
import           Data.Version
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                      hiding (FilePath)
import           System.CPUTime
import           Text.Printf

import           Paths_popvox_scrape
import           PopVox.OpenSecrets
import           PopVox.OpenSecrets.Types
import           PopVox.Types


type Translator a = Conduit Record (ResourceT IO) (Either String a)

newtype HashIndex k v = HashIndex { getIndex :: M.HashMap k v }

instance (Eq k, Hashable k, Monoid v) => Monoid (HashIndex k v) where
    mempty = HashIndex mempty
    mappend (HashIndex m1) (HashIndex m2) = HashIndex $ M.unionWith mappend m1 m2

type IndexAmount = (Sum Int, HashIndex RecipientType (Sum Int))

type PartyIndex a = HashIndex Party (Sum a)
type OrgIndex a   = HashIndex T.Text (PartyIndex a)


main :: IO ()
main = do
    PopVoxOptions{..} <- execParser opts

    start <- getCPUTime

    i <- indexFile (_popVoxOpenSecretsDir </> "indivs12.txt")  indexIndiv
    e <- indexFile (_popVoxOpenSecretsDir </> "expends12.txt") indexExp
    let allOrgs = L.sort . S.toList $ orgs i `S.union` orgs e

    runResourceT $  CL.sourceList allOrgs
                 $= CL.map (toMapRow i e)
                 $= (writeHeaders defCSVSettings >> fromCSV defCSVSettings)
                 $$ sinkFile (encodeString _popVoxOutput)

    end <- getCPUTime
    when _popVoxVerbose $ do
        let elapsed = (fromIntegral (end - start)) / ((10^12) :: Double)
        F.print  "Elapsed: {} sec\n" $ F.Only elapsed

orgs :: OrgIndex a -> S.HashSet T.Text
orgs = S.fromList . M.keys . getIndex

indexFile :: (FromRecord a, Num b)
          => FilePath -> (Either String a -> OrgIndex b) -> IO (OrgIndex b)
indexFile input f =  runResourceT
                  $  readOpenSecretsC input
                  $= parseOpenSecrets
                  $$ CL.foldMap f

indexIndiv :: Either String Individual -> OrgIndex Int
indexIndiv (Left _) = mempty
indexIndiv (Right Individual{..}) =
    maybe mempty (orgIndex name _indAmount) $ getParty' _indRecipCode
    where
        name = T.strip _indOrgName

orgIndex :: T.Text -> a -> Party -> OrgIndex a
orgIndex org amount party = HashIndex
                          . M.singleton org
                          . HashIndex
                          . M.singleton party
                          $ Sum amount

indexExp :: Either String Expenditure -> OrgIndex Double
indexExp (Left _) = mempty
indexExp (Right Expenditure{..}) =
    maybe mempty (orgIndex name _expAmount) $ getParty' _expRecipCode
    where
        name = T.strip _expCRPRecipName

getParty' :: Maybe RecipientType -> Maybe Party
getParty' = (getParty =<<)

getParty :: RecipientType -> Maybe Party
getParty (CandidateR p _)    = Just p
getParty (CommitteeR p)      = Just p
getParty (PACR _ )           = Nothing
getParty (OutsideSpending _) = Nothing

type DonationReport a = (T.Text, Party, a, Party, a)

toList :: Num a => OrgIndex a -> [DonationReport a]
toList (HashIndex orgi) = do
    (org, HashIndex partyi) <- M.toList orgi
    return ( org
           , Dem
           , getSum $ M.lookupDefault mempty Dem partyi
           , Rep
           , getSum $ M.lookupDefault mempty Rep partyi
           )

toMapRow :: OrgIndex Int -> OrgIndex Double -> T.Text -> MapRow BS.ByteString
toMapRow indivs expends org =
    ML.fromList [ ("Organization", encodeUtf8 org)
                , ("Dem12",    toField $ lu indivs  org Dem)
                , ("GOP12",    toField $ lu indivs  org Rep)
                , ("DemInd12", toField $ lu expends org Dem)
                , ("GOPInd12", toField $ lu expends org Rep)
                ]
    where
        lu :: Num a => OrgIndex a -> T.Text -> Party -> a
        lu m o p =   getSum . fold
                 $   M.lookup p . getIndex
                 =<< M.lookup o (getIndex m)

testOpenSecrets :: (FromRecord a, Show a)
                => FilePath -> Translator a -> IO ()
testOpenSecrets filepath translator = do
    print filepath
    start <- getCPUTime
    (errs, oks) <- fmap (bimap getSum getSum) $ runResourceT $
        readOpenSecretsC filepath $= translator $$ CL.foldMapM accum
    end <- getCPUTime
    let elapsed = ((fromIntegral (end - start)) / ((10^(12 :: Int)) :: Double)) :: Double
    putStrLn $ "ERROR COUNT: " ++ show (errs :: Int)
    putStrLn $ "OK    COUNT: " ++ show (oks :: Int)
    printf     "Elapsed    : %0.3f sec\n" (elapsed :: Double)
    putStrLn ""

    where
        accum (Left err) =  liftIO (putStrLn $ "ERROR: " ++ err)
                         >> return (Sum 1, mempty)
        accum (Right _)  = return (mempty, Sum 1)

opts' :: Parser PopVoxOptions
opts' =   PopVoxOptions
      <$> fileOpt (  short 's'
                  <> long "opensecrets"
                  <> metavar "DIRNAME"
                  <> help "The location of the opensecrets.org data sets.")
      <*> fileOpt (  short 'o'
                  <> long "output"
                  <> metavar "OUTPUT_FILE"
                  <> help "The CSV filename to write the data to.")
      <*> switch  (  short 'v'
                  <> long "verbose"
                  <> help "Print extra information.")

opts :: ParserInfo PopVoxOptions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Scrapes data from popvox.com and \
                        \mashes it with opensecrets.org data."
            <> header ("popvox-scrape v" ++ showVersion version))

fileOpt :: Mod OptionFields FilePath -> Parser FilePath
fileOpt = option (pure . decodeString)
