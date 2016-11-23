module WebP.Encode
  ( Width(..)
  , Height(..)
  , Quality(..)
  , encodeRGB
  , encodeBGR
  , encodeRGBA
  , encodeBGRA
  , losslessEncodeRGB
  , losslessEncodeBGR
  , losslessEncodeRGBA
  , losslessEncodeBGRA
  )

where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import           Foreign
import           Foreign.C.Types
import           System.IO.Unsafe
import           WebP.FFI.Encode
import           WebP.Types

encode :: InputFormat -> ByteString -> Width -> Height -> Quality -> ByteString
encode input bytes (Width width) (Height height) (Quality quality) =
  let (Image _ bytes') =
        unsafePerformIO $ encode' input bytes width height quality
  in bytes'

encode' :: InputFormat -> ByteString -> Int -> Int -> Float -> IO Image
encode' input bytes width height quality =
  Bytes.useAsCStringLen bytes $ \(bytesPtr, _) ->
    alloca $ \outPtr -> do
      len <-
        encodingFunction
          input
          (castPtr bytesPtr)
          (CInt (fromIntegral width))
          (CInt (fromIntegral height))
          (CInt (fromIntegral (width * 3)))
          (CFloat quality)
          outPtr
      out <- peek outPtr
      fr <- newForeignPtr finalizerFree out
      bs <-
        withForeignPtr fr $ \x ->
          Bytes.packCStringLen (castPtr x, fromIntegral len)
      return $ Image fr bs

encodingFunction
  :: InputFormat
  -> Ptr CUChar
  -> CInt
  -> CInt
  -> CInt
  -> CFloat
  -> Ptr (Ptr CUChar)
  -> IO Int
encodingFunction RGB = c_WebPEncodeRGB
encodingFunction BGR = c_WebPEncodeBGR
encodingFunction RGBA = c_WebPEncodeRGBA
encodingFunction BGRA = c_WebPEncodeBGRA

encodeRGB :: ByteString -> Width -> Height -> Quality -> ByteString
encodeRGB = encode RGB

encodeBGR :: ByteString -> Width -> Height -> Quality -> ByteString
encodeBGR = encode BGR

encodeRGBA :: ByteString -> Width -> Height -> Quality -> ByteString
encodeRGBA = encode RGBA

encodeBGRA :: ByteString -> Width -> Height -> Quality -> ByteString
encodeBGRA = encode BGRA

losslessEncode :: InputFormat -> ByteString -> Width -> Height -> ByteString
losslessEncode input bytes (Width width) (Height height) =
  unsafePerformIO $
  Bytes.useAsCStringLen bytes $ \(bytesPtr, _) ->
    alloca $ \outPtr -> do
      len <-
        losslessEncodingFunction
          input
          (castPtr bytesPtr)
          (CInt (fromIntegral width))
          (CInt (fromIntegral height))
          (CInt (fromIntegral (width * 3)))
          outPtr
      out <- peek outPtr
      Bytes.packCStringLen (castPtr out, fromIntegral len)

losslessEncodingFunction
  :: InputFormat
  -> Ptr CUChar
  -> CInt
  -> CInt
  -> CInt
  -> Ptr (Ptr CUChar)
  -> IO Int
losslessEncodingFunction RGB = c_WebPEncodeLosslessRGB
losslessEncodingFunction BGR = c_WebPEncodeLosslessBGR
losslessEncodingFunction RGBA = c_WebPEncodeLosslessRGBA
losslessEncodingFunction BGRA = c_WebPEncodeLosslessBGRA

losslessEncodeRGB :: ByteString -> Width -> Height -> ByteString
losslessEncodeRGB = losslessEncode RGB

losslessEncodeBGR :: ByteString -> Width -> Height -> ByteString
losslessEncodeBGR = losslessEncode BGR

losslessEncodeRGBA :: ByteString -> Width -> Height -> ByteString
losslessEncodeRGBA = losslessEncode RGBA

losslessEncodeBGRA :: ByteString -> Width -> Height -> ByteString
losslessEncodeBGRA = losslessEncode BGRA
