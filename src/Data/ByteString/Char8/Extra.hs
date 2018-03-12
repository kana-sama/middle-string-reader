module Data.ByteString.Char8.Extra
  ( substring
  , indexedLines
  , indexedLinesBackward
  ) where

import           Prelude               hiding (drop, length, take)

import           Data.ByteString.Char8 (ByteString, drop, elemIndices, length,
                                        take, unsnoc)

substring :: Int -> Int -> ByteString -> ByteString
substring from to = take (to - from) . drop from

indexedLines :: ByteString -> [(Int, ByteString)]
indexedLines string = zipWith zipper (-1:indices) indices where
  zipper from to = (from + 1, substring (from + 1) to string)
  indices = elemIndices '\n' string

indexedLinesBackward :: ByteString -> [(Int, ByteString)]
indexedLinesBackward string = go (length string) (length string) string where
  go from to s = case unsnoc s of
    Nothing           -> []
    Just (init, '\n') -> (from, substring from to string) : go (from - 1) from init
    Just (init, last) -> go (from - 1) to init
