{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WebPSpec where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           WebP

data Input =
  Input ByteString
  deriving (Show)

instance Arbitrary Input where
  arbitrary =
    Input <$>
    arbitrary `suchThat`
    (\s ->
       Bytes.length s `mod` 3 == 0 &&
       Bytes.length s `mod` 2 == 0 && Bytes.length s > 0)

data InputWithAlpha =
  InputWithAlpha ByteString
  deriving (Show)

data PixelRGBA =
  PixelRGBA ByteString
  deriving (Show)

-- This ensures alpha is greater than 0 so that the pixel is visible, otherwise
-- it is zeroed out after decoding. It makes testing a bit easier.
instance Arbitrary PixelRGBA where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    a <- arbitrary `suchThat` (> 0)
    return $ PixelRGBA $ Bytes.pack [r, g, b, a]

toBS :: PixelRGBA -> ByteString
toBS (PixelRGBA bs) = bs

instance Arbitrary InputWithAlpha where
  arbitrary = do
    (pixels :: [PixelRGBA]) <-
      arbitrary `suchThat` (\x -> length x > 2 && length x `mod` 2 == 0)
    return $ InputWithAlpha $ Bytes.concat (map toBS pixels)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "WebP" $ do
    modifyMaxSize (const 10000) $ do
      it "losslessEncodeRGB . decodeRGB  == id" $
        property $ \(Input bytes) ->
          let len = Bytes.length bytes
              !encoded =
                losslessEncodeRGB bytes (Width (len `div` 3 `div` 2)) (Height 2)
              !decoded = decodeRGB encoded
          in decoded == bytes
      it "for visible pixels: losslessEncodeRGBA . decodeRGBA  == id" $
        property $ \(InputWithAlpha bytes) ->
          let len = Bytes.length bytes
              !encoded =
                losslessEncodeRGBA
                  bytes
                  (Width (len `div` 4 `div` 2))
                  (Height 2)
              !decoded = decodeRGBA encoded
          in decoded == bytes
