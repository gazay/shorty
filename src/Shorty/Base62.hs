module Shorty.Base62 where

import qualified Data.List as List

chars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

encodeChar :: Int -> Char
encodeChar a = chars !! a

decodeChar :: (Int, Char) -> Int
decodeChar (step, a) = case List.elemIndex a chars of
                         Nothing -> error ("Symbol out of range: " ++ [a])
                         Just val -> val * (62^step)

encode :: Int -> String
encode 0 = ""
encode a = let d = div a 62
               r = rem a 62
             in
               reverse $ encodeChar r : encode d

decode :: String -> Int
decode "" = 0
decode xs = let withIndexes = zip [0..(length xs - 1)] xs
              in
                sum $ map decodeChar withIndexes
