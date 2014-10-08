{-# LANGUAGE OverloadedStrings #-}


module PopVox.OpenSecrets.Utils
    ( char
    , given
    ) where


import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as B


char :: Char -> A.Parser B.ByteString
char = fmap B.singleton . A.word8 . toEnum . fromEnum

given :: A.Parser a -> A.Parser (Maybe a)
given p = fmap Just p <|> (char ' ' *> pure Nothing)
