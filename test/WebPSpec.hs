{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WebPSpec
  (spec)

where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import           Data.Word
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           WebP

instance Arbitrary Bytes.ByteString where
    arbitrary = do
      (x :: [Word8]) <- listOf (elements [0..255])
      return $  Bytes.pack x
    shrink xs = Bytes.pack <$> shrink (Bytes.unpack xs)

divisors
  :: forall b.
     Integral b
  => b -> [b]
divisors n = 1 : filter ((== 0) . rem n) [2 .. n `div` 2]

data ImageRGB =
  ImageRGB ByteString
           Width
           Height
  deriving (Show)

instance Arbitrary ImageRGB where
  arbitrary = do
    bytes <-
      arbitrary `suchThat`
      (\s -> Bytes.length s `mod` 3 == 0 && Bytes.length s > 0)
    let divs = divisors $ Bytes.length bytes `div` 3
    w <- elements divs
    let width = Bytes.length bytes `div` 3 `div` w
        height = Bytes.length bytes `div` 3 `div` width
    return $ ImageRGB bytes (Width width) (Height height)

data ImageRGBA =
  ImageRGBA ByteString Width Height
  deriving (Show)

data PixelRGBA =
  PixelRGBA ByteString
  deriving (Show)

-- This ensures alpha is greater than 0 so that the pixel is visible, otherwise
-- it is zeroed out after decoding. It makes testing a bit easier.
instance Arbitrary PixelRGBA where
  arbitrary = do
    r <- elements [0..255]
    g <- elements [0..255]
    b <- elements [0..255]
    a <- elements [1..255]
    return $ PixelRGBA $ Bytes.pack [r, g, b, a]

toBS :: PixelRGBA -> ByteString
toBS (PixelRGBA bs) = bs

instance Arbitrary ImageRGBA where
  arbitrary = do
    (pixels :: [PixelRGBA]) <-
      arbitrary `suchThat` (\x -> length x > 2 && length x `mod` 2 == 0)
    let bytes = Bytes.concat (map toBS pixels)
    let divs = divisors $ Bytes.length bytes `div` 4
    w <- elements divs
    let width = Bytes.length bytes `div` 4 `div` w
        height = Bytes.length bytes `div` 4 `div` width
    return $ ImageRGBA bytes (Width width) (Height height)

{-# ANN spec "HLint: ignore Redundant do" #-}
spec :: Spec
spec = do
  describe "WebP" $ do
    modifyMaxSize (const 10000) $ do
      it "losslessEncodeRGB . decodeRGB  == id" $
        property $ \(ImageRGB bytes width height) ->
          let !encoded = losslessEncodeRGB bytes width height
              !decoded = decodeRGB encoded
          in decoded == bytes
      it "for visible pixels: losslessEncodeRGBA . decodeRGBA  == id" $
        property $ \(ImageRGBA bytes width height) ->
          let !encoded = losslessEncodeRGBA bytes width height
              !decoded = decodeRGBA encoded
          in decoded == bytes
      it "getInfo computes correct width and height" $
        property $ \(ImageRGB bytes width height) ->
          let info = getInfo (losslessEncodeRGB bytes width height)
          in case info of
               Nothing -> False
               Just (w, h) -> w == width && h == height
