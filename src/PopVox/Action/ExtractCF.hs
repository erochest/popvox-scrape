{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module PopVox.Action.ExtractCF
    ( extractCF
    ) where


import           Conduit
import           Control.Applicative
import           Control.Error
import qualified Data.ByteString.Lazy      as B
import           Data.Csv
import           Data.Csv.Conduit
import           Data.Hashable
import qualified Data.HashSet              as S
import           Data.Monoid
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           GHC.Generics
import           Prelude                   hiding (FilePath)

import           PopVox.Types              hiding (cfOutputFile,
                                            contribDataFile)


data ContribCFData = CFData ContribEntry T.Text (Maybe CFScore)
                   deriving (Show, Eq, Generic)

instance Hashable ContribCFData

instance FromNamedRecord ContribCFData where
    parseNamedRecord m =
        CFData <$> parseNamedRecord m
               <*> m .: "recipient_name"
               <*> m .: "candidate_cfscore"

instance ToNamedRecord ContribCFData where
    toNamedRecord (CFData ce n cf) =
        toNamedRecord ce <> namedRecord [ "recipient_name"    .= n
                                        , "candidate_cfscore" .= cf
                                        ]

extractCF :: FilePath -> FilePath -> Script ()
extractCF contribDataFile cfOutputFile = do
    cfs <- runResourceT $  sourceFile contribDataFile
                        $= fromNamedCsvLiftError toErr defaultDecodeOptions
                        $= filterC keep
                        $$ foldlC (flip S.insert) S.empty
    scriptIO . B.writeFile (encodeString cfOutputFile)
             . encodeByName header
             . S.toList
             $ cfs
    where
        header = [ "recipient_name"
                 , "candidate_cfscore"
                 ]


keep :: ContribCFData -> Bool
keep (CFData (Candidate _ Dem) _ _) = True
keep (CFData (Candidate _ GOP) _ _) = True
keep _                              = False

toErr :: CsvParseError -> String
toErr (CsvParseError input er) =  "PARSE ERROR: " ++ er
                               ++ ": <<<" ++ show input ++ ">>>"
toErr (IncrementalError er)    =  "INCREMENTAL ERROR: " ++ er
