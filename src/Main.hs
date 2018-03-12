{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.ByteString.Char8.Extra (indexedLines,
                                              indexedLinesBackward)
import           Data.List                   (find, intercalate)

import           System.Environment          (getArgs)
import           System.IO.MMap              (mmapFileByteString)

main :: IO ()
main = getArgs >>= \case
  ["gen"] -> do
    print "[Generate input]"
    writeFile "input" . intercalate "\n" . fmap show $ [1..999999]
  _       -> do
    print "[Searching line]"
    input <- mmapFileByteString "input" Nothing
    print . fmap extractLine . find isSameLine $ zip (indexedLines input) (indexedLinesBackward input)
  where
    isSameLine ((ix1, _), (ix2, _)) = ix1 >= ix2
    extractLine ((_, line), _) = line
