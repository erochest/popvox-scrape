{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}


module PopVox.Types
    ( PopVoxOptions(..)
    , popVoxOpenSecretsDir
    , popVoxOutput
    , popVoxAPI
    , popVoxVerbose

    , Bill(..)
    , billID
    , billCongress
    , billTitle
    , billType
    , billNumber
    , billURL

    , Org(..)
    , orgID
    , orgName
    , orgURL

    , OrgPosition(..)
    , positionOrg
    , positionBill
    , position

    , Position

    , HashIndex(..)

    , APIKey(..)
    , BillID
    , OrgID

    , PopVoxSessionT(..)
    , runPopVox
    , hoistEither
    , hoistMaybe
    ) where


import           Control.Applicative
import           Control.Error             hiding (hoistEither, hoistMaybe)
import qualified Control.Error             as E
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import qualified Data.ByteString           as BS
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import           Data.Monoid
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)


type BillID = Int
type OrgID  = Int

newtype APIKey = APIKey { getAPIKey :: T.Text }
                 deriving (Show)

data Bill = Bill
          { _billID       :: !BillID
          , _billCongress :: !Int
          , _billTitle    :: !T.Text
          , _billType     :: !T.Text
          , _billNumber   :: !Int
          , _billURL      :: !String
          } deriving (Show)
makeLenses ''Bill

data Org = Org
         { _orgID   :: !OrgID
         , _orgName :: !T.Text
         , _orgURL  :: !String
         } deriving (Show)
makeLenses ''Org

data Position = PosPositive
              | PosNegative
              | PosNeutral
              deriving (Show)
makePrisms ''Position

data OrgPosition = OrgPos
                 { _positionOrg  :: !OrgID
                 , _positionBill :: !BillID
                 , _position     :: !Position
                 } deriving (Show)
makeLenses ''OrgPosition

data PopVoxOptions = PopVoxOptions
    { _popVoxOpenSecretsDir :: !FilePath
    , _popVoxOutput         :: !FilePath
    , _popVoxAPI            :: !APIKey
    , _popVoxVerbose        :: !Bool
    } deriving (Show)
makeLenses ''PopVoxOptions

newtype HashIndex k v = HashIndex { getIndex :: M.HashMap k v }

instance (Eq k, Hashable k, Monoid v) => Monoid (HashIndex k v) where
    mempty = HashIndex mempty
    mappend (HashIndex m1) (HashIndex m2) = HashIndex $ M.unionWith mappend m1 m2

newtype PopVoxSessionT m a
    = PopVoxSessionT
    { unPopVox :: ReaderT APIKey (EitherT SomeException m) a
    } deriving (Functor, Applicative, Monad)

instance Monad m => MonadReader APIKey (PopVoxSessionT m) where
    ask = PopVoxSessionT ask
    local f m = PopVoxSessionT . ReaderT $ runReaderT (unPopVox m) . f

instance (Functor m, MonadIO m) => MonadIO (PopVoxSessionT m) where
    liftIO = PopVoxSessionT . ReaderT . const . EitherT . fmap Right . liftIO

instance Monad m => MonadError SomeException (PopVoxSessionT m) where
    throwError = PopVoxSessionT . ReaderT . const . EitherT . return . Left
    catchError m handler = wrap $ \r ->
        unwrap r m `catchError` (unwrap r . handler)
        where
            unwrap k n = runReaderT (unPopVox n) k
            wrap       = PopVoxSessionT . ReaderT

runPopVox :: APIKey -> PopVoxSessionT m a -> m (Either SomeException a)
runPopVox k m = runEitherT $ runReaderT (unPopVox m) k

hoistEither :: Monad m => Either SomeException a -> PopVoxSessionT m a
hoistEither = PopVoxSessionT . ReaderT . const . E.hoistEither

hoistMaybe :: Monad m => SomeException -> Maybe a -> PopVoxSessionT m a
hoistMaybe e = hoistEither . note e
