{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Pretty (
  Pretty(..)
, PrettyPrint(..)
) where

import           Data.Yaml
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.String

newtype Pretty a = Pretty {unPretty :: a}
  deriving (Functor, Eq, Ord, IsString)
 
instance PrettyPrint a => Show (Pretty a) where
  show = prettyPrint . unPretty

class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Value where
  prettyPrint = T.unpack . decodeUtf8 . encode
