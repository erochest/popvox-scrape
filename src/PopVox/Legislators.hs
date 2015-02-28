{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}


module PopVox.Legislators
    ( readLegislatorIndex
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.Aeson.Lens
import           Data.Foldable
import qualified Data.HashMap.Strict       as M
import qualified Data.Text                 as T
import qualified Data.Text.Read            as TR
import           Data.Yaml
import           Filesystem.Path.CurrentOS hiding (concat)
import           Prelude                   hiding (FilePath, concatMap)

import           PopVox.Types


readLegislatorIndex :: FilePath -> Script LegislatorIndex
readLegislatorIndex = EitherT
                    . fmap (show `bimap` legislator)
                    . decodeFileEither
                    . encodeString

_TInt :: Prism' T.Text Int
_TInt = prism' (T.pack . show)
               (\t -> case TR.decimal t of
                        Right (n, rest) | T.null rest -> Just n
                                        | otherwise   -> Nothing
                        Left _ -> Nothing)

legislator :: Value -> LegislatorIndex
legislator v = M.fromList . concatMap legPair $ v ^.. _Array . traverse

strkey :: forall t (f :: * -> *). (AsValue t, Applicative f)
       => T.Text -> (T.Text -> f T.Text) -> t -> f t
strkey k = key k . _String

intkey :: forall t (f :: * -> *) a
       .  (AsValue t, Contravariant f, Applicative f, Num a)
       => T.Text -> (a -> f a) -> t -> f t
intkey k = key k . _Integer . to fromIntegral

legPair :: Value -> [(Thomas, LegislatorInfo ICPSR)]
legPair r = mapMaybe ( uncurry (liftA2 (,))
                     . (r ^? key "id" . strkey "thomas" . to Thomas,)
                     . legInfo (r ^? key "id" . intkey "icpsr" . to ICPSR)
                     )
          $ r ^.. key "terms" . _Array . traverse

legInfo :: Maybe ICPSR -> Value -> Maybe (LegislatorInfo ICPSR)
legInfo icpsr r =   LegInfo
                <$> icpsr
                <*> r ^? strkey "start" . to (T.take 4) . _TInt
                <*> join (r ^? strkey "party" . to party)

party :: T.Text -> Maybe Party
party "Democrat"   = Just Dem
party "Republican" = Just GOP
party _            = Nothing
