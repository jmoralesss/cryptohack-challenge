{-
    Cryptosystems like RSA works on numbers, but messages are made up of characters. How should we convert our messages into numbers so that mathematical operations can be applied?

    The most common way is to take the ordinal bytes of the message, convert them into hexadecimal, and concatenate. This can be interpreted as a base-16 number, and also represented in base-10.

    To illustrate:

        message: HELLO
        ascii bytes: [72, 69, 76, 76, 79]
        hex bytes: [0x48, 0x45, 0x4c, 0x4c, 0x4f]
        base-16: 0x48454c4c4f
        base-10: 310400273487

    Python's PyCryptodome library implements this with the methods Crypto.Util.number.bytes_to_long and Crypto.Util.number.long_to_bytes.

    Convert the following integer back into a message:

    11515195063862318899931685488813747395775516287289682636499965282714637259206269
-}

import Data.List.Split (chunksOf)
import Text.XML.HXT.DOM.Util (hexStringToInt)
import Data.Char (chr)

main :: IO ()
main = 
    print decimalToChar
    where
        decimalToChar = map chr hexToDecimal
        hexToDecimal = map hexStringToInt hexChunks
        hexChunks = chunksOf 2 decimalToHex
        decimalToHex = toHex input ""
        input = 11515195063862318899931685488813747395775516287289682636499965282714637259206269

decimalToHex :: Integer -> String
decimalToHex x
    | x `elem` [0..9] = show x
decimalToHex 10 = "A"
decimalToHex 11 = "B"
decimalToHex 12 = "C"
decimalToHex 13 = "D"
decimalToHex 14 = "E"
decimalToHex 15 = "F"

toHex :: Integer -> String -> String
toHex remainder acc
    | remainder == 0 = acc
    | otherwise = toHex flooredRemainder hexAccumulator
    where
        flooredRemainder = remainder `div` 16
        hexAccumulator = (decimalToHex (remainder `mod` 16)) ++ acc