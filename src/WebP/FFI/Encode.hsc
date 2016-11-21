{-# LANGUAGE CPP #-}
{-# ForeignFunctionInterface #-}

module WebP.FFI.Encode
  ( c_WebPEncodeRGB
  , c_WebPEncodeBGR
  , c_WebPEncodeRGBA
  , c_WebPEncodeBGRA
  , c_WebPEncodeLosslessRGB
  , c_WebPEncodeLosslessBGR
  , c_WebPEncodeLosslessRGBA
  , c_WebPEncodeLosslessBGRA
  )

where

import           Foreign
import           Foreign.C.Types

#include <webp/encode.h>

foreign import ccall unsafe "webp/encode.h WebPEncodeRGB"
               c_WebPEncodeRGB ::
               Ptr CUChar ->
                 CInt -> CInt -> CInt -> CFloat -> Ptr (Ptr CUChar) -> IO Int

foreign import ccall unsafe "webp/encode.h WebPEncodeBGR"
               c_WebPEncodeBGR ::
               Ptr CUChar ->
                 CInt -> CInt -> CInt -> CFloat -> Ptr (Ptr CUChar) -> IO Int

foreign import ccall unsafe "webp/encode.h WebPEncodeRGBA"
               c_WebPEncodeRGBA ::
               Ptr CUChar ->
                 CInt -> CInt -> CInt -> CFloat -> Ptr (Ptr CUChar) -> IO Int

foreign import ccall unsafe "webp/encode.h WebPEncodeBGRA"
               c_WebPEncodeBGRA ::
               Ptr CUChar ->
                 CInt -> CInt -> CInt -> CFloat -> Ptr (Ptr CUChar) -> IO Int

-- lossless versions

foreign import ccall unsafe "webp/encode.h WebPEncodeLosslessRGB"
               c_WebPEncodeLosslessRGB ::
               Ptr CUChar ->
                 CInt -> CInt -> CInt -> Ptr (Ptr CUChar) -> IO Int

foreign import ccall unsafe "webp/encode.h WebPEncodeLosslessBGR"
               c_WebPEncodeLosslessBGR ::
               Ptr CUChar ->
                 CInt -> CInt -> CInt -> Ptr (Ptr CUChar) -> IO Int

foreign import ccall unsafe "webp/encode.h WebPEncodeLosslessRGBA"
               c_WebPEncodeLosslessRGBA ::
               Ptr CUChar ->
                 CInt -> CInt -> CInt -> Ptr (Ptr CUChar) -> IO Int

foreign import ccall unsafe "webp/encode.h WebPEncodeLosslessBGRA"
               c_WebPEncodeLosslessBGRA ::
               Ptr CUChar ->
                 CInt -> CInt -> CInt -> Ptr (Ptr CUChar) -> IO Int
