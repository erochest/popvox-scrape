{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module PopVox.OpenSecrets.Types.Common
    ( Year

    , Party(..)
    , _Dem
    , _Rep
    , _Ind
    , _Lib
    , _Thr
    , _PUnk

    , CandidateStatus(..)
    , _Win
    , _Los
    , _Inc
    , _Cha
    , _Opn
    , _Non

    , ContribType(..)
    , _Bus
    , _Lab
    , _Ideo
    , _PACOther
    , _PACUnk

    , RecipientType(..)
    , _CandidateR
    , _CommitteeR
    , _PACR
    , _OutsideSpending

    , District(..)
    , TransactionType(..)
    , ElectionType(..)
    , isValue

    , PartyIndex
    , getParty
    , getParty'

    , OrgIndex
    , orgs
    , orgIndex
    ) where


import           Control.Applicative
import           Control.DeepSeq
import           Control.DeepSeq.Generics
import           Control.Error
import           Control.Lens
import           Control.Monad
import qualified Data.Attoparsec.ByteString  as A
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as C8
import           Data.CSV.Conduit.Conversion
import           Data.Hashable
import qualified Data.HashMap.Strict         as M
import qualified Data.HashSet                as S
import           Data.Monoid
import qualified Data.Text                   as T
import           Data.Text.Encoding
import           Data.Time
import           Data.Word
import           GHC.Generics                (Generic)
import           System.Locale

import           PopVox.OpenSecrets.Utils
import           PopVox.Types


isValue :: B.ByteString -> B.ByteString -> Bool
isValue a b = a == b

type Year = Int

data Party = Dem
           | Rep
           | Ind
           | Lib
           | Thr
           | PUnk
           deriving (Show, Eq, Bounded, Generic)
makePrisms ''Party

instance NFData Party

instance Hashable Party

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
                     deriving (Show, Eq, Bounded, Generic)
makePrisms ''CandidateStatus

instance Hashable CandidateStatus

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
                 deriving (Show, Eq, Bounded, Generic)
makePrisms ''ContribType

instance Hashable ContribType

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
        deriving (Show, Eq, Generic)
makePrisms ''RecipientType

instance Hashable RecipientType

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
                        <|> char 'b' *> return Bus
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
        either mkErr return $ parseRecipientType field
        where mkErr m = fail $ "Invalid RecipientType ('" ++ C8.unpack field ++ "'): " ++ m

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

data TransactionType = TransContribution
                     | TransEarmarked
                     | TransCommittee
                     | TransRefund
                     | TransRefundCandidate
                     | TransSoft
                     | TransCode T.Text
                     | TransIndExpenditureAgainst
                     | TransCoordExpenditureAgainst
                     | TransIndExpenditureFor
                     | TransCommCostFor
                     | TransAffiliatedCmte
                     | TransDirect
                     | TransCommCostAgainst
                     | TransRecountDisb
                     | TransInKind
                     | TransTribal
                     deriving (Show, Eq)

instance FromField TransactionType where
    parseField "10"  = pure TransSoft
    parseField "10 " = pure TransSoft
    parseField "11"  = pure TransTribal
    parseField "11 " = pure TransTribal
    parseField "15"  = pure TransContribution
    parseField "15 " = pure TransContribution
    parseField "15e" = pure TransEarmarked
    parseField "15E" = pure TransEarmarked
    parseField "15j" = pure TransCommittee
    parseField "15J" = pure TransCommittee
    parseField "22y" = pure TransRefund
    parseField "22Y" = pure TransRefund
    parseField "22z" = pure TransRefundCandidate
    parseField "22Z" = pure TransRefundCandidate
    parseField "24A" = pure TransIndExpenditureAgainst
    parseField "24C" = pure TransCoordExpenditureAgainst
    parseField "24E" = pure TransCoordExpenditureAgainst
    parseField "24F" = pure TransCommCostFor
    parseField "24G" = pure TransAffiliatedCmte
    parseField "24K" = pure TransDirect
    parseField "24N" = pure TransCommCostAgainst
    parseField "24R" = pure TransRecountDisb
    parseField "24Z" = pure TransInKind
    parseField t     = pure . TransCode $ decodeLatin1 t

data ElectionType = ElectionPrimary !(Maybe Year)
                  | ElectionGeneral !(Maybe Year)
                  | ElectionSpecial !(Maybe Year)
                  | ElectionRunoff  !(Maybe Year)
                  | ElectionOther   !(Maybe Year)
                  | ElectionUnknown !(Maybe Char) !Year
                  | ElectionRaw     !T.Text
                  deriving (Show, Eq)
makePrisms ''ElectionType

parseElectionType :: B.ByteString -> ElectionType
parseElectionType et =
      either (const . ElectionRaw . T.strip $ decodeLatin1 et) id
    . join
    . fmap (note "")
    $ A.parseOnly (skipSpaces *> electionType <* skipSpaces <* A.endOfInput) et

electionType :: A.Parser (Maybe ElectionType)
electionType = A.option (fmap (ElectionUnknown Nothing)) code <*> year

code :: A.Parser (Maybe Year -> Maybe ElectionType)
code =   (char 'P' *> pure (Just . ElectionPrimary))
     <|> (char 'G' *> pure (Just . ElectionGeneral))
     <|> (char 'O' *> pure (Just . ElectionOther))
     <|> (char 'R' *> pure (Just . ElectionRunoff))
     <|> (char 'S' *> pure (Just . ElectionSpecial))
     <|> (unknown <$> A.satisfy isLetter)

year :: A.Parser (Maybe Int)
year = do
    digits <- numbers
    return $ if B.length digits == 4
        then Just (B.foldl' accumYear 0 digits)
        else Nothing

numbers :: A.Parser B.ByteString
numbers = A.takeWhile isDigit

accumYear :: Int -> Word8 -> Int
accumYear accum w = accum * 10 + (fromIntegral w - 48)

unknown :: Word8 -> Maybe Year -> Maybe ElectionType
unknown w y = ElectionUnknown (Just . toEnum $ fromEnum w) <$> y

skipSpaces :: A.Parser ()
skipSpaces = A.skipWhile isSpace

isLetter, isDigit, isSpace :: Word8 -> Bool
isLetter l = l >= 65 && l <= 90
isDigit d  = d >= 48 && d <= 57
isSpace s  = s == 32 || s == 9 || s == 10 || s == 13

instance FromField ElectionType where
    parseField field = return $ parseElectionType field

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

instance FromField Day where
    parseField bs =
        maybe errMsg pure $ parseTime defaultTimeLocale "%m/%d/%Y" (C8.unpack bs)
        where errMsg = fail $ "Invalid date: '" ++ C8.unpack bs ++ "'"

type PartyIndex a = HashIndex Party (Sum a)

getParty' :: Maybe RecipientType -> Maybe Party
getParty' = (getParty =<<)

getParty :: RecipientType -> Maybe Party
getParty (CandidateR p _)    = Just p
getParty (CommitteeR p)      = Just p
getParty (PACR _ )           = Nothing
getParty (OutsideSpending _) = Nothing

type OrgIndex a   = HashIndex T.Text (PartyIndex a)

orgs :: OrgIndex a -> S.HashSet T.Text
orgs = S.fromList . M.keys . getIndex

orgIndex :: T.Text -> a -> Party -> OrgIndex a
orgIndex org amount party = HashIndex
                          . M.singleton org
                          . HashIndex
                          . M.singleton party
                          $ Sum amount

instance NFData a => NFData (Sum a)
