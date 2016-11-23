{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module WebP.FFI.Decode
  ( c_WebPDecodeRGB
  , c_WebPDecodeBGR
  , c_WebPDecodeRGBA
  , c_WebPDecodeBGRA
  , c_WebPGetInfo
  )

where

import Foreign
import Foreign.C
import Foreign.C.Types

#include <webp/decode.h>

foreign import ccall unsafe "webp/decode.h WebPDecodeRGB"
               c_WebPDecodeRGB ::
               Ptr CUChar ->
                 CSize -> Ptr CInt -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall unsafe "webp/decode.h WebPDecodeBGR"
               c_WebPDecodeBGR ::
               Ptr CUChar ->
                 CSize -> Ptr CInt -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall unsafe "webp/decode.h WebPDecodeRGBA"
               c_WebPDecodeRGBA ::
               Ptr CUChar ->
                 CSize -> Ptr CInt -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall unsafe "webp/decode.h WebPDecodeBGRA"
               c_WebPDecodeBGRA ::
               Ptr CUChar ->
                 CSize -> Ptr CInt -> Ptr CInt -> IO (Ptr CUChar)

foreign import ccall unsafe "webp/decode.h WebPGetInfo"
               c_WebPGetInfo ::
               Ptr CUChar -> CSize -> Ptr CInt -> Ptr CInt -> IO Int
