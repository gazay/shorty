module Shortener

import qualified System.Random as SR

codeChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

randomElement :: [a] -> IO a
randomElement xs = do
                    let maxIndex = (length xs) - 1
                    randomDigit <- SR.randomRIO (0, maxIndex)
                    return $ xs !! randomDigit

codeGen :: Int -> IO [Char]
codeGen n = replicate n (randomElement codeChars)


