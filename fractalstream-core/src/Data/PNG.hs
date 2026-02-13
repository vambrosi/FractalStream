{-# language OverloadedStrings #-}
module Data.PNG
  ( toPng
  , bufferToPng
  , encodeToPngFile
  , encodeBufferToPngFile
  ) where

-- Based on https://wiki.haskell.org/Library/PNG

import Data.Array
import Data.Bits
import Data.Word
import qualified Codec.Compression.Zlib as Z
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe
import Foreign.Ptr

be8 :: Word8 -> ByteString
be8 x = BSL.singleton x

be32 :: Word32 -> ByteString
be32 x = BSL.pack [fromIntegral (x `shiftR` sh) | sh <- [24,16,8,0]]

--pack :: String -> ByteString
--pack xs = BSL.pack $ map (fromIntegral.fromEnum) xs

hdr, iHDR, iDAT, iEND :: ByteString
hdr = "\137\80\78\71\13\10\26\10"
iHDR = "IHDR"
iDAT = "IDAT"
iEND = "IEND"

chunk :: ByteString -> ByteString -> [ByteString]
chunk tag xs = [be32 (fromIntegral $ BSL.length xs), dat, be32 (crc dat)]
    where dat = BSL.append tag xs

toPng :: (Word32, Word32) -> BS.ByteString -> ByteString
toPng (width, height) dat = BSL.concat $ hdr : concat [ihdr, imgdat, iend]
     where ihdr = chunk iHDR $ BSL.concat
                     [ be32 width
                     , be32 height
                     , be8 8   -- bits per sample (8 for r, 8 for g, 8 for b)
                     , be8 2   -- color type (2=rgb)
                     , be8 0   -- compression method
                     , be8 0   -- filter method
                     , be8 0 ] -- interlace method
           imgdat = chunk iDAT (Z.compress imagedata)
           imagedata = insertFilterBytes width (BSL.fromStrict dat)
           iend = chunk iEND BSL.empty

bufferToPng :: (Word32, Word32) -> Ptr Word8 -> IO ByteString
bufferToPng (w, h) ptr = toPng (w, h) <$> unsafePackCStringLen (castPtr ptr, fromIntegral (3 * w * h))

encodeToPngFile :: (Word32, Word32) -> BS.ByteString -> FilePath -> IO ()
encodeToPngFile dim dat path = BSL.writeFile path (toPng dim dat)

encodeBufferToPngFile :: (Word32, Word32) -> Ptr Word8 -> FilePath -> IO ()
encodeBufferToPngFile dim dat path = BSL.writeFile path =<< bufferToPng dim dat

insertFilterBytes :: Word32 -> ByteString -> ByteString
insertFilterBytes width = go
  where
    w = fromIntegral width
    go xs
      | BSL.null xs = ""
      | otherwise   = let (h, t) = BSL.splitAt (3 * w) xs in BSL.cons 0 (h <> go t)

crc :: ByteString -> Word32
crc xs = updateCrc 0xffffffff xs `xor` 0xffffffff

updateCrc :: Word32 -> ByteString -> Word32
updateCrc = BSL.foldl' crcStep

crcStep :: Word32 -> Word8 -> Word32
crcStep x ch = (crcTab ! n) `xor` (x `shiftR` 8)
    where n = fromIntegral (x `xor` fromIntegral ch)

crcTab :: Array Word8 Word32
crcTab = listArray (0,255) $ flip map [0..255] (\n ->
    foldl' (\c _ -> if c .&. 1 == 1
                      then 0xedb88320 `xor` (c `shiftR` 1)
                      else c `shiftR` 1) n [0..7 :: Int])
