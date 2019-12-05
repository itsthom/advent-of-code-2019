module Utils
    ( replaceAt,
      splitStr
    ) where

import qualified Data.Text as T

replaceAt :: Eq a => [a] -> Int -> a -> [a]
replaceAt source index newValue =
  x ++ newValue : ys
  where (x,_:ys) = splitAt index source

splitStr :: String -> String -> [String]
splitStr delimiter input =
  map T.unpack textLst
  where textLst = T.splitOn (T.pack delimiter) (T.pack input)