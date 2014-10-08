{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module PopVox.OpenSecrets.Types.Common
    ( Year
    , Party(..)
    , CandidateStatus(..)
    , ContribType(..)
    , RecipientType(..)
    , District(..)
    ) where


import Data.Text.Encoding
import qualified Data.Text as T
import           Control.Applicative
import           Control.Lens
import qualified Data.Attoparsec.ByteString  as A
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as C8
import           Data.CSV.Conduit.Conversion

import           PopVox.OpenSecrets.Utils


type Year = Int

data Party = Dem
           | Rep
           | Ind
           | Lib
           | Thr
           | PUnk
           deriving (Show, Eq, Bounded)
makePrisms ''Party

instance FromField Party where
    parseField "D" = return Dem
    parseField "R" = return Rep
    parseField "I" = return Ind
    parseField "L" = return Lib
    parseField "3" = return Thr
    parseField "U" = return PUnk
    parseField p   = fail $ "Invalid Party: " ++ C8.unpack p

data CandidateStatus = Win
                     | Los
                     | Inc
                     | Cha
                     | Opn
                     | Non
                     deriving (Show, Eq, Bounded)
makePrisms ''CandidateStatus

instance FromField CandidateStatus where
    parseField "W" = return Win
    parseField "L" = return Los
    parseField "I" = return Inc
    parseField "C" = return Cha
    parseField "O" = return Opn
    parseField "N" = return Non
    parseField _   = fail "Invalid CandidateStatus"

data ContribType = Bus
                 | Lab
                 | Ideo
                 | PACOther
                 | PACUnk
                 deriving (Show, Eq, Bounded)
makePrisms ''ContribType

instance FromField ContribType where
    parseField "B" = return Bus
    parseField "L" = return Lab
    parseField "I" = return Ideo
    parseField "O" = return PACOther
    parseField "U" = return PACUnk
    parseField _   = fail "Invalid ContribType"

data RecipientType
        = CandidateR !Party !(Maybe CandidateStatus)
        | CommitteeR !Party
        | PACR !ContribType
        | OutsideSpending !ContribType
        deriving (Show, Eq)
makePrisms ''RecipientType

parseRecipientType :: B.ByteString -> Either String RecipientType
parseRecipientType = A.parseOnly (recipientType <* A.endOfInput)
    where
        recipientType   = candidateR <|> committeeR <|> pacr <|> outsideSpending
        candidateR      = CandidateR <$> party <*> given candidateStatus
        committeeR      = (CommitteeR <$> party) <* char 'P'
        pacr            = char 'P' *> (PACR <$> contribType)
        outsideSpending = char 'O' *> (OutsideSpending <$> contribType)
        party           =   char 'D' *> return Dem
                        <|> char 'R' *> return Rep
                        <|> char 'I' *> return Ind
                        <|> char 'L' *> return Lib
                        <|> char '3' *> return Thr
                        <|> char 'U' *> return PUnk
        candidateStatus =   char 'W' *> return Win
                        <|> char 'L' *> return Los
                        <|> char 'I' *> return Inc
                        <|> char 'C' *> return Cha
                        <|> char 'O' *> return Opn
                        <|> char 'N' *> return Non
        contribType     =   char 'B' *> return Bus
                        <|> char 'b' *> return Lab
                        <|> char 'L' *> return Lab
                        <|> char 'l' *> return Lab
                        <|> char 'I' *> return Ideo
                        <|> char 'i' *> return Ideo
                        <|> char 'O' *> return PACOther
                        <|> char 'o' *> return PACOther
                        <|> char 'U' *> return PACUnk
                        <|> char 'u' *> return PACUnk

instance FromField RecipientType where
    parseField field =
        either (const err) return $ parseRecipientType field
        where err = fail $ "Invalid RecipientType: '" ++ C8.unpack field ++ "'"

data District
        = House !T.Text !T.Text
        | Senate1
        | Senate2
        | President
        deriving (Show, Eq)
makeLenses ''District
makePrisms ''District

instance FromField District where
    parseField "S1"   = pure Senate1
    parseField "S2"   = pure Senate2
    parseField "PRES" = pure President
    parseField f
        | B.length f == 4 = pure . uncurry House . T.splitAt 2 $ decodeUtf8 f
        | otherwise       = fail $ "Invalid District: '" ++ C8.unpack f ++ "'"

instance FromField Bool where
    parseField "R" = return True        -- ^ This is slightly arbitrary.
    parseField "Y" = return True
    parseField "y" = return True
    parseField "1" = return True
    parseField "T" = return True
    parseField "t" = return True
    parseField "N" = return False
    parseField "n" = return False
    parseField "0" = return False
    parseField "F" = return False
    parseField "f" = return False
    parseField " " = return False
    parseField ""  = return False
    parseField b   = fail $ "Invalid Bool: '" ++ C8.unpack b ++ "'"
