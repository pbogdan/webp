-- | WebP decoding and encoding functions.
--
-- The inputs of encoding functions and outputs of decoding functions are arrays
-- of bytes representing pixels, with one byte per channel.
--
-- For example functions operating on arrays in RGB layout expect them to be in
-- the following format:
--
-- @
--     import qualified Data.ByteString as Bytes
--     import Data.Word
--
--     Bytes.pack ([r, g, b, r, g, b, ..] :: Word8)
-- @
module WebP
  ( module WebP.Types
  , module WebP.Encode
  , module WebP.Decode
  )

where

import WebP.Decode
import WebP.Encode
import WebP.Types hiding (Image(..))
