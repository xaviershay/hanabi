{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hanabi.Extras.Aeson
  ( StripPrefix (..)
  , LowercaseShow (..)
  ) where

import Data.Aeson

import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic, Rep, datatypeName, from)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

newtype LowercaseShow a = LowercaseShow a
instance Show a => ToJSON (LowercaseShow a) where
  toJSON (LowercaseShow x) = toJSON . map toLower . show $ x
instance (Show a, Enum a, Bounded a) => FromJSON (LowercaseShow a) where
  parseJSON = withText typename $ \input ->
    case M.lookup input allMembers of
      Just x  -> return (LowercaseShow x)
      Nothing -> fail $ "unknown " <> typename

    where
      typename = datatypeName (from (Proxy :: Proxy a))
      allMembers = M.fromList $
        map (\x -> (T.pack . map toLower $ show x, x)) [minBound :: a .. ]

-- |
-- Strip a prefix from each record field name. Use with via deriving.
newtype StripPrefix (s :: Symbol) a =
  StripPrefix a

lowercaseFirst (x:xs) = toLower x:xs
lowercaseFirst [] = []

instance ( Generic a
         , GFromJSON Zero (Rep a)
         , KnownSymbol s )
      => FromJSON (StripPrefix s a) where
  {-# INLINE parseJSON #-}
  parseJSON = fmap StripPrefix . genericParseJSON options
    where options = defaultOptions { fieldLabelModifier = drop' }
          drop'   = lowercaseFirst
                      . (fromMaybe <*> stripPrefix (symbolVal (Proxy @s)))

instance ( Generic a
         , GToJSON Zero (Rep a)
         , KnownSymbol s )
      => ToJSON (StripPrefix s a) where
  toJSON (StripPrefix x) = genericToJSON options x
    where options = defaultOptions { fieldLabelModifier = drop' }
          drop'   = lowercaseFirst
                      . (fromMaybe <*> stripPrefix (symbolVal (Proxy @s)))
