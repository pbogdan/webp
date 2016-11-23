{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module WebP.Decode
  ( decodeRGB
  , decodeBGR
  , decodeRGBA
  , decodeBGRA
  , getInfo
  )

where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import           Foreign
import           Foreign.C.Types
import           System.IO.Unsafe
import           WebP.FFI.Decode
import           WebP.Types

decode :: InputFormat -> ByteString -> ByteString
decode input bytes =
  let (Image _ bytes') = unsafePerformIO $ decode' input bytes
  in bytes'

decode' :: InputFormat -> ByteString -> IO Image
decode' input bytes =
  Bytes.useAsCStringLen bytes $ \(bytesPtr, l) ->
    alloca $ \widthPtr ->
      alloca $ \heightPtr -> do
        !outPtr <-
          decodingFunction
            input
            (castPtr bytesPtr)
            (fromIntegral l)
            widthPtr
            heightPtr
        width <- peek widthPtr
        height <- peek heightPtr
        fr <- newForeignPtr finalizerFree outPtr
        bs <-
          withForeignPtr fr $ \x ->
            Bytes.packCStringLen
              (castPtr x, fromIntegral width * fromIntegral height * 3)
        return $ Image fr bs

decodingFunction :: InputFormat
                 -> Ptr CUChar
                 -> CSize
                 -> Ptr CInt
                 -> Ptr CInt
                 -> IO (Ptr CUChar)
decodingFunction RGB = c_WebPDecodeRGB
decodingFunction BGR = c_WebPDecodeBGR
decodingFunction RGBA = c_WebPDecodeRGBA
decodingFunction BGRA = c_WebPDecodeBGRA

decodeRGB :: ByteString -> ByteString
decodeRGB = decode RGB
decodeBGR :: ByteString -> ByteString
decodeBGR = decode BGR
decodeRGBA :: ByteString -> ByteString
decodeRGBA = decode RGBA
decodeBGRA :: ByteString -> ByteString
decodeBGRA = decode BGRA

getInfo :: ByteString -> Maybe (Width, Height)
getInfo bytes =
  unsafePerformIO $
  Bytes.useAsCStringLen bytes $ \(bytesPtr, l) ->
    alloca $ \widthPtr ->
      alloca $ \heightPtr -> do
        ret <-
          c_WebPGetInfo (castPtr bytesPtr) (fromIntegral l) widthPtr heightPtr
        case ret of
          0 -> return Nothing
          _ -> do
            width <- peek widthPtr
            height <- peek heightPtr
            return $
              Just (Width (fromIntegral width), Height (fromIntegral height))
