{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Shmonad.Expression.Types where

import qualified Data.Text.Lazy as L
import Data.Char
import Data.Monoid
import Data.String (IsString, fromString)
import Data.Number.Nat
import System.Exit (ExitCode(..))

default (L.Text)

type Str = L.Text
type UniqueID = Nat

newtype Name = Name Str
  deriving Eq

instance Show Name where
  show (Name n) = L.unpack n

data VarID a = VarID
  { varID   :: Maybe UniqueID -- must have this, unless coming from environment
  , varName :: Name
  }

-- | A Variable has a unique name.
class Variable a where
  uniqueName :: VarID a -> Name 
  uniqueName (VarID vID vName) 
    = toName $ fromName vName <> 
        case vID of
          Just n -> L.pack (show n)
          Nothing -> "" 

instance Variable Str
instance Variable Integer

-- | A standard FilePath (i.e. a string).
newtype Path = Path FilePath
  deriving Eq

instance Show Path where
  show (Path fp) = show fp

instance Variable Path

newtype ShBool = ShBool ExitCode

instance Show ShBool where
  show (ShBool b) = case b of
    ExitSuccess   -> "(true)" 
    ExitFailure _ -> "(false)"

instance Variable ShBool

-- | A Variable may or may not be set when used as a value.
-- Programs using environment variables should handle both
-- cases explicitly.
instance Variable a => Variable (Maybe a)

-- | ShShow defaults to the typical Show instance, but can be customized
-- for different variable types.

class (Variable a, Show a) => ShShow a where
  toShString :: a -> Str
  toShString = L.pack . show

instance ShShow Str where
  toShString = id

instance ShShow Integer
instance ShShow Path

instance IsString Name where
  fromString = toName . L.pack

instance ShShow a => ShShow (Maybe a) where
  toShString (Just x) = toShString x
  toShString Nothing = "\"\""

data StrSum = forall a. ShShow a => StrSum { getCat :: a }

instance Show StrSum where
  show (StrSum x) = show x

instance Variable StrSum

instance Monoid StrSum where
  mempty = StrSum ("" :: Str)
  StrSum x `mappend` StrSum y = StrSum $ toShString x <> toShString y

instance ShShow StrSum where
  toShString (StrSum x) = toShString x

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
