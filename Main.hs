{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Main where


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

type PartyIndex = HashIndex Party (Sum Int)
type OrgIndex   = HashIndex T.Text PartyIndex


main :: IO ()
main = do
    PopVoxOptions{..} <- execParser opts

    start <- getCPUTime

    i <- runResourceT $
        readOpenSecretsC (_popVoxOpenSecretsDir </> "indivs12.txt")
                 $=  parseOpenSecrets
                 $$ CL.foldMap indexIndiv
    runResourceT $  CL.sourceList (toList i)
                 $= CL.map toMapRow
                 $= (writeHeaders defCSVSettings >> fromCSV defCSVSettings)
                 $$ sinkFile (encodeString _popVoxOutput)

    end <- getCPUTime
    when _popVoxVerbose $ do
        let elapsed = (fromIntegral (end - start)) / ((10^12) :: Double)
        F.print  "Elapsed: {} sec\n" $ F.Only elapsed

instance ToField Party where
    toField Dem  = "D12"
    toField Rep  = "R12"
    toField Ind  = "I12"
    toField Lib  = "L12"
    toField Thr  = "T12"
    toField PUnk = "U12"

indexIndiv :: Either String Individual -> OrgIndex
indexIndiv (Left _) = mempty
indexIndiv (Right Individual{..}) =
        maybe mempty (orgIndex orgName _indAmount)
    $   getParty
    =<< _indRecipCode
    where
        orgName = T.strip _indOrgName

        getParty (CandidateR p _)    = Just p
        getParty (CommitteeR p)      = Just p
        getParty (PACR _ )           = Nothing
        getParty (OutsideSpending _) = Nothing

        orgIndex org amount party = HashIndex
                                  . M.singleton org
                                  . HashIndex
                                  . M.singleton party
                                  $ Sum amount

type DonationReport = (T.Text, Party, Int, Party, Int)

toList :: OrgIndex -> [DonationReport]
toList (HashIndex orgi) = do
    (org, HashIndex partyi) <- M.toList orgi
    return ( org
           , Dem
           , maybe 0 getSum $ M.lookup Dem partyi
           , Rep
           , maybe 0 getSum $ M.lookup Rep partyi
           )

toMapRow :: DonationReport -> MapRow BS.ByteString
toMapRow (org, p1, a1, p2, a2) =
    ML.fromList [ ("Organization", encodeUtf8 org)
                , (toField p1,     toField a1)
                , (toField p2,     toField a2)
                ]

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
