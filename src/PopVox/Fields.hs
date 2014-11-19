

module PopVox.Fields
    ( Fields(..)
    , reader
    , asint
    , asdouble
    , pnote
    , field
    ) where


import           Control.Applicative
import qualified Data.Text                 as T
import           Data.Text.Read


newtype Fields a b = Fields { runFields :: a -> Either String b }

field :: Int -> Int -> Fields T.Text T.Text
field i l = Fields $ Right . T.take l . T.drop i

reader :: String -> Reader b -> T.Text -> Fields a b
reader e r t =
    case r (T.strip t) of
        Right (n, x)
            | T.null x  -> return n
            | otherwise -> fail $  e
                                ++ ": Invalid input. Unexpected leftovers: "
                                ++ T.unpack x
        Left l          -> fail l

asint :: T.Text -> Fields a Int
asint = reader "DECIMAL" decimal

asdouble :: T.Text -> Fields a Double
asdouble = reader "DOUBLE" double

pnote :: String -> Fields a (Maybe b) -> Fields a b
pnote e = (>>= maybe (fail e) return)

instance Functor (Fields a) where
    fmap f (Fields p) = Fields $ fmap f . p

instance Applicative (Fields a) where
    pure = Fields . const . Right
    (Fields f) <*> (Fields x) = Fields $ \a ->
        f a <*> x a

instance Monad (Fields a) where
    return = Fields . const . Right
    fail = Fields . const . Left
    (Fields x) >>= f = Fields $ \a ->
        case x a of
            Right x' -> runFields (f x') a
            Left e   -> Left e
