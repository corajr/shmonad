{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Shmonad.Expression.Types where

import qualified Data.Text.Lazy as L
import Data.Char
import Data.Monoid ((<>))
import Data.String (IsString, fromString)
import Data.Number.Nat
import System.Exit (ExitCode(..))

default (L.Text)

type Str = L.Text
type UniqueID = Nat

newtype Name = Name Str
  deriving (Eq, Show)

data VarID a = VarID
  { varID   :: UniqueID
  , varName :: Name
  } deriving (Eq, Show)

-- | A Variable has a unique name.
class (Show a, Eq a) => Variable a where
  uniqueName :: VarID a -> Name 
  uniqueName (VarID vID vName) = toName $ fromName vName <> L.pack (show vID)

instance Variable Str
instance Variable Integer

-- | A standard FilePath (i.e. a string).
newtype Path = Path FilePath
  deriving (Eq)

instance Show Path where
  show (Path fp) = show fp

instance Variable Path

newtype ShBool = ShBool ExitCode
  deriving Eq

instance Show ShBool where
  show (ShBool b) = case b of
    ExitSuccess   -> "(true)" 
    ExitFailure _ -> "(false)"

instance Variable ShBool

instance IsString Name where
  fromString = toName . L.pack

isValidName :: Str -> Bool
isValidName x = nonEmpty && allValidChars && notStartWithNumber
  where nonEmpty = not (L.null x)
        allValidChars = L.all (\c -> isAlphaNum c || c == '_') x
        notStartWithNumber = not . isDigit $ L.head x 

toName :: Str -> Name
toName x
  | isValidName x = Name x
  | otherwise = error $ L.unpack ("\"" <> x <> "\" is not a valid shell name.")

fromName :: Name -> Str
fromName (Name x) = x
