{-# LANGUAGE BangPatterns#-}

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

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "WebP" $ do
    modifyMaxSize (const 10000) $ do
      it "losslessEncode . decode  == id" $
        property $ \(Input bytes) ->
          let len = Bytes.length bytes
              !encoded =
                losslessEncodeRGB bytes (Width (len `div` 3 `div` 2)) (Height 2)
              !decoded = decodeRGB encoded
          in decoded == bytes
