{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebP.Types
  (Image(..)
  , Width(..)
  , Height(..)
  , Quality(..)
  , InputFormat(..)
  , Alpha(..)
  , BitstreamFeatures(..)
  )

where

import Data.ByteString (ByteString)
import Foreign
import Foreign.C.Types

data Image =  Image !(ForeignPtr CUChar) !(ByteString)


newtype Width =
  Width Int
  deriving (Eq, Num, Ord, Show)

newtype Height =
  Height Int
  deriving (Eq, Num, Ord, Show)

newtype Quality =
  Quality Float
  deriving (Eq, Num, Ord, Show)

data InputFormat
  = RGB
  | BGR
  | RGBA
  | BGRA
  deriving (Eq, Show)


data Alpha
  = HasAlpha
  | NoAlpha
  deriving (Eq, Show)

data BitstreamFeatures =
  BitstreamFeatures Width
                    Height
                    Alpha
  deriving (Eq, Show)
