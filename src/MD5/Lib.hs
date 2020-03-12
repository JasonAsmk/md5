module MD5.Lib where

import           Control.Monad.State.Lazy
import           Data.Bits
import qualified Data.ByteString.Lazy     as L
import           Data.Deserializer        (fromBytes)
import           Data.Maybe               (fromJust)
import           Data.Serializer          (toBytes)
import qualified Data.Vector.Unboxed      as U
import           Data.Word
import           Text.Printf


fileToVector :: FilePath -> IO (U.Vector Word8)
fileToVector path = do
  byteStr <- L.readFile path
  return $ U.fromList $ L.unpack byteStr


md5 :: FilePath -> IO String
md5 path =
  do
    v <- fileToVector path
    let byteMessage = (padTo448 v) <> keepLower64Bits (bitLength v)
    return $ digestMessage $ processMessage $ word8VecToWord32Vec byteMessage

word8VecToWord32Vec :: U.Vector Word8 -> U.Vector Word32
word8VecToWord32Vec v = let nWord32 = U.length v `div` 4
                            word8ToWord32 i = fromJust $ fromBytes $ U.toList $ U.slice (4*i) 4 v -- this can throw an exception, also the Word32 goes from low to high bytes
                        in
                          foldr (\a b -> b <> U.singleton (word8ToWord32 a)) U.empty [0..(nWord32-1)]

digestMessage :: MD5Data -> String
digestMessage st = foldr (\a b -> b <> a) "" $ map (printf "%02x") $ (toBytes $ a st) <> (toBytes $ b st) <> (toBytes $ c st) <> (toBytes $ d st)

processMessage :: U.Vector Word32 -> MD5Data
processMessage v = let nBlocks = U.length v `div` 16
                       initialMD5Data = MD5Data {
                                          a = 0x01234567,
                                          b = 0x89abcdef,
                                          c = 0xfedcba98,
                                          d = 0x76543210,
                                          x = U.slice 0 16 v
                                        }
                   in
                    foldr (\i st -> processBlock st { x = U.slice (i*16) 16 v }) initialMD5Data [0..(nBlocks - 1)]

processBlock :: MD5Data -> MD5Data
processBlock st = let st' = round4 (round3 (round2 (round1 st)))
                  in
                    MD5Data { a = a st + a st', b = b st + b st', c = c st + c st', d = d st + d st', x = x st }

f :: Word32 -> Word32 -> Word32 -> Word32
f x y z = (x .&. y) .|. (complement x .&. z)

g :: Word32 -> Word32 -> Word32 -> Word32
g x y z = (x .&. z) .|. (y .&. complement z)

h :: Word32 -> Word32 -> Word32 -> Word32
h x y z = x `xor` y `xor` z

i :: Word32 -> Word32 -> Word32 -> Word32
i x y z = y `xor` (x .|. complement z)

data MD5Data = MD5Data {
  a :: Word32, -- register A
  b :: Word32, -- register B
  c :: Word32, -- register C
  d :: Word32, -- register D
  x :: U.Vector Word32 -- 16 element array, 512 bit part of message
}

transformF :: TransformFunction
transformF x1 x2 x3 x4 xk s ti = x2 + shift (x1 + f x2 x3 x4 + xk + ti) s

transformG :: TransformFunction
transformG x1 x2 x3 x4 xk s ti = x2 + shift (x1 + g x2 x3 x4 + xk + ti) s

transformH :: TransformFunction
transformH x1 x2 x3 x4 xk s ti = x2 + shift (x1 + h x2 x3 x4 + xk + ti) s

transformI :: TransformFunction
transformI x1 x2 x3 x4 xk s ti = x2 + shift (x1 + i x2 x3 x4 + xk + ti) s

type TransformStep = (
  Int,
  MD5Data -> Word32,
  MD5Data -> Word32,
  MD5Data -> Word32,
  MD5Data -> Word32,
  Int,
  Int,
  Word32
                        )

type TransformFunction = (Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Int -> Word32 -> Word32)

roundStep :: TransformFunction -> TransformStep -> MD5Data -> MD5Data
roundStep f (pos, a', b', c', d', k, s, ti) st
  =
     let newValue = f (a' st) (b' st) (c' st) (d' st) (x st U.! k) s ti
     in
      case pos of
        1 -> st { a = newValue }
        2 -> st { b = newValue }
        3 -> st { c = newValue }
        4 -> st { d = newValue }

round1 :: MD5Data -> MD5Data
round1 st = foldr (roundStep transformF) st [
  (1, a, b, c, d, 0, 7, 0xd76aa478),
  (4, d, a, b, c, 1, 12, 0xe8c7b756),
  (3, c, d, a, b, 2, 17, 0x242070db),
  (2, b, c, d, a, 3, 22, 0xc1bdceee),
  (1, a, b, c, d, 4, 7, 0xf57c0faf),
  (4, d, a, b, c, 5, 12, 0x4787c62a),
  (3, c, d, a, b, 6, 17, 0xa8304613),
  (2, b, c, d, a, 7, 22, 0xfd469501),
  (1, a, b, c, d, 8, 7, 0x698098d8),
  (4, d, a, b, c, 9, 12, 0x8b44f7af),
  (3, c, d, a, b, 10, 17, 0xffff5bb1),
  (2, b, c, d, a, 11, 22, 0x895cd7be),
  (1, a, b, c, d, 12, 7, 0x6b901122),
  (4, d, a, b, c, 13, 12, 0xfd987193),
  (3, c, d, a, b, 14, 17, 0xa679438e),
  (2, b, c, d, a, 15, 22, 0x49b40821)
                        ]

round2 :: MD5Data -> MD5Data
round2 st = foldr (roundStep transformG) st [
  (1, a, b, c, d, 1, 5, 0xf61e2562),
  (4, d, a, b, c, 6, 9, 0xc040b340),
  (3, c, d, a, b, 11, 14, 0x265e5a51),
  (2, b, c, d, a, 0, 20, 0xe9b6c7aa),
  (1, a, b, c, d, 5, 5, 0xd62f105d),
  (4, d, a, b, c, 10, 9, 0x2441453),
  (3, c, d, a, b, 15, 14, 0xd8a1e681),
  (2, b, c, d, a, 4, 20, 0xe7d3fbc8),
  (1, a, b, c, d, 9, 5, 0x21e1cde6),
  (4, d, a, b, c, 14, 9, 0xc33707d6),
  (3, c, d, a, b, 3, 14, 0xf4d50d87),
  (2, b, c, d, a, 8, 20, 0x455a14ed),
  (1, a, b, c, d, 13, 5, 0xa9e3e905),
  (4, d, a, b, c, 2, 9, 0xfcefa3f8),
  (3, c, d, a, b, 7, 14, 0x676f02d9),
  (2, b, c, d, a, 12, 20, 0x8d2a4c8a)
                        ]

round3 :: MD5Data -> MD5Data
round3 st = foldr (roundStep transformH) st [
  (1, a, b, c, d, 5, 4, 0xfffa3942),
  (4, d, a, b, c, 8, 11, 0x8771f681),
  (3, c, d, a, b, 11, 16, 0x6d9d6122),
  (2, b, c, d, a, 14, 23, 0xfde5380c),
  (1, a, b, c, d, 1, 4, 0xa4beea44),
  (4, d, a, b, c, 4, 11, 0x4bdecfa9),
  (3, c, d, a, b, 7, 16, 0xf6bb4b60),
  (2, b, c, d, a, 10, 23, 0xbebfbc70),
  (1, a, b, c, d, 13, 4, 0x289b7ec6),
  (4, d, a, b, c, 0, 11, 0xeaa127fa),
  (3, c, d, a, b, 3, 16, 0xd4ef3085),
  (2, b, c, d, a, 6, 23, 0x4881d05),
  (1, a, b, c, d, 9, 4, 0xd9d4d039),
  (4, d, a, b, c, 12, 11, 0xe6db99e5),
  (3, c, d, a, b, 15, 16, 0x1fa27cf8),
  (2, b, c, d, a, 2, 23, 0xc4ac5665)
                        ]

round4 :: MD5Data -> MD5Data
round4 st = foldr (roundStep transformI) st [
  (1, a, b, c, d, 0, 6, 0xf4292244),
  (4, d, a, b, c, 7, 10, 0x432aff97),
  (3, c, d, a, b, 14, 15, 0xab9423a7),
  (2, b, c, d, a, 5, 21, 0xfc93a039),
  (1, a, b, c, d, 12, 6, 0x655b59c3),
  (4, d, a, b, c, 3, 10, 0x8f0ccc92),
  (3, c, d, a, b, 10, 15, 0xffeff47d),
  (2, b, c, d, a, 1, 21, 0x85845dd1),
  (1, a, b, c, d, 8, 6, 0x6fa87e4f),
  (4, d, a, b, c, 15, 10, 0xfe2ce6e0),
  (3, c, d, a, b, 6, 15, 0xa3014314),
  (2, b, c, d, a, 13, 21, 0x4e0811a1),
  (1, a, b, c, d, 4, 6, 0xf7537e82),
  (4, d, a, b, c, 11, 10, 0xbd3af235),
  (3, c, d, a, b, 2, 15, 0x2ad7d2bb),
  (2, b, c, d, a, 9, 21, 0xeb86d391)
                        ]

bitLength :: U.Vector Word8 -> Int
bitLength v = U.length v * 8

padTo448 :: U.Vector Word8 -> U.Vector Word8
padTo448 v =
  let originalSize = bitLength v
      remainder = originalSize `mod` 512
  in
    if remainder < 448
       then
         U.snoc v 0x80 <> U.replicate (((448 - remainder) `div` 8) - 1) 0x00
       else
         U.snoc v 0x80 <> U.replicate (((512 - remainder) `div` 8) - 1 + (448 `div` 8)) 0x00

keepLower64Bits :: Int -> U.Vector Word8
keepLower64Bits x =
  let  v = U.fromList (toBytes x)
  in
    U.generate 8 (\n -> v U.! n)
