{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK prune #-}

module WebP.Types
  (Image(..)
  , Width(..)
  , Height(..)
  , Quality(..)
  , InputFormat(..)
  , stride
  )

where

import Data.ByteString (ByteString)
import Foreign
import Foreign.C.Types

data Image =  Image !(ForeignPtr CUChar) !ByteString

-- | Represents width of an image.
newtype Width =
  Width Int
  deriving (Eq, Num, Ord, Show)

-- | Represents height of an image.
newtype Height =
  Height Int
  deriving (Eq, Num, Ord, Show)

-- | Represents quality to be used in encoding operations. Valid ranges are
-- between 0 and 100.
newtype Quality =
  Quality Float
  deriving (Eq, Num, Ord, Show)

data InputFormat
  = RGB
  | BGR
  | RGBA
  | BGRA
  deriving (Eq, Show)

stride :: InputFormat -> Int
stride RGB = 3
stride BGR = 3
stride RGBA = 4
stride BGRA = 4
