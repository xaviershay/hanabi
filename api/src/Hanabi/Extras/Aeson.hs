{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hanabi.Extras.Aeson
  ( StripPrefix (..)
  ) where

import Data.Aeson

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

-- |
-- Strip a prefix from each record field name. Use with via deriving.
newtype StripPrefix (s :: Symbol) a =
  StripPrefix a

instance ( Generic a
         , GFromJSON Zero (Rep a)
         , KnownSymbol s )
      => FromJSON (StripPrefix s a) where
  {-# INLINE parseJSON #-}
  parseJSON = fmap StripPrefix . genericParseJSON options
    where options = defaultOptions { fieldLabelModifier = drop' }
          drop'   = fromMaybe <*> stripPrefix (symbolVal (Proxy @s))
