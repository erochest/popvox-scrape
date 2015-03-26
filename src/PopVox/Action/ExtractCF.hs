{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module PopVox.Action.ExtractCF
    ( extractCF
    ) where


import           Conduit
import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy      as B
import           Data.Csv
import qualified Data.Csv                  as Csv
import           Data.Csv.Conduit
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Tuple                (swap)
import           Data.Yaml                 hiding ((.:), (.=))
import           Filesystem.Path.CurrentOS
import           GHC.Generics              hiding (to)
import           Prelude                   hiding (FilePath)

import           PopVox.Types              hiding (cfOutputFile,
                                            contribDataFile)


data ContribCFData = CFData ContribEntry T.Text (Maybe CFScore)
                   deriving (Show, Eq, Generic)

instance Hashable ContribCFData

instance FromNamedRecord ContribCFData where
    parseNamedRecord m =
        CFData <$> parseNamedRecord m
               <*> m Csv..: "recipient_name"
               <*> m Csv..: "candidate_cfscore"

instance ToNamedRecord ContribCFData where
    toNamedRecord (CFData ce n cf) =
        toNamedRecord ce <> namedRecord [ "recipient_name"    Csv..= n
                                        , "candidate_cfscore" Csv..= cf
                                        ]

data CFThomas = CFThomas ContribCFData (Maybe Thomas)

instance FromNamedRecord CFThomas where
    parseNamedRecord m =
        CFThomas <$> parseNamedRecord m
                 <*> optional (fmap Thomas (m Csv..: "thomas_id"))

instance ToNamedRecord CFThomas where
    toNamedRecord (CFThomas cfd t) =
        toNamedRecord cfd <> namedRecord [ "thomas_id" Csv..= fmap unThomas t ]

extractCF :: FilePath -> FilePath -> Script ()
extractCF contribDataFile cfOutputFile = do
    cfs <- runResourceT $  sourceFile contribDataFile
                        $= fromNamedCsvLiftError toErr defaultDecodeOptions
                        $= filterC keep
                        $$ foldlC (flip S.insert) S.empty
    legs <-  M.fromList . fmap (swap . fmap T.toLower)
         <$> readLegNames "ids/legislators-current.yaml"
    scriptIO . B.writeFile (encodeString cfOutputFile)
             . encodeByName header
             . map (toCFThomas legs)
             $ S.toList cfs
    where
        header = [ "recipient_name"
                 , "candidate_cfscore"
                 , "thomas_id"
                 ]

toCFThomas :: M.HashMap T.Text Thomas -> ContribCFData -> CFThomas
toCFThomas m cfd@(CFData _ n _) = CFThomas cfd $ M.lookup (T.toLower n) m

keep :: ContribCFData -> Bool
keep (CFData (Candidate _ Dem) _ _) = True
keep (CFData (Candidate _ GOP) _ _) = True
keep _                              = False

toErr :: CsvParseError -> String
toErr (CsvParseError input er) =  "PARSE ERROR: " ++ er
                               ++ ": <<<" ++ show input ++ ">>>"
toErr (IncrementalError er)    =  "INCREMENTAL ERROR: " ++ er

readLegNames :: FilePath -> Script [(Thomas, T.Text)]
readLegNames = EitherT
             . fmap (show `bimap` legislators)
             . decodeFileEither
             . encodeString

mboth :: Applicative m => (a -> m b) -> (a -> m c) -> (b -> c -> d) -> a -> m d
mboth f g h a = h <$> f a <*> g a

legislators :: Value -> [(Thomas, T.Text)]
legislators v = mapMaybe (mboth legThomas legName (,)) $ v ^.. _Array . traverse

legName :: Value -> Maybe T.Text
legName v = fmap T.toLower $ joinName <$> v ^? key "name" . strkey "last"
                                      <*> v ^? key "name" . strkey "first"
    where
        joinName l f = l <> ", " <> f

legThomas :: Value -> Maybe Thomas
legThomas v = v ^? key "id" . strkey "thomas" . to Thomas

strkey :: forall t (f :: * -> *). (AsValue t, Applicative f)
       => T.Text -> (T.Text -> f T.Text) -> t -> f t
strkey k = key k . _String
