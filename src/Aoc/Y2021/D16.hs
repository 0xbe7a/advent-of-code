module Aoc.Y2021.D16
  ( day
  ) where

import           Control.Monad

import           Aoc.Day
import           Aoc.Parse
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.Text (Text, pack)
import Control.Applicative (Applicative(liftA2))

data Header = Header {version :: Int, typeId :: Int} deriving Show
data Payload = Value Int | Operator [Packet] deriving Show
data Packet = Packet {header :: Header, payload :: Payload} deriving Show

hexDigitToBin '0' = "0000"
hexDigitToBin '1' = "0001"
hexDigitToBin '2' = "0010"
hexDigitToBin '3' = "0011"
hexDigitToBin '4' = "0100"
hexDigitToBin '5' = "0101"
hexDigitToBin '6' = "0110"
hexDigitToBin '7' = "0111"
hexDigitToBin '8' = "1000"
hexDigitToBin '9' = "1001"
hexDigitToBin 'A' = "1010"
hexDigitToBin 'B' = "1011"
hexDigitToBin 'C' = "1100"
hexDigitToBin 'D' = "1101"
hexDigitToBin 'E' = "1110"
hexDigitToBin 'F' = "1111"
hexDigitToBin _ = error "Invalid Char"

parseBinDigit :: Int -> Int -> Parser Int
parseBinDigit n p = bin2Num p <$> count n binDigitChar
    where bin2Num p = foldl' (\sum i -> sum * 2 + digitToInt i) p
    
parseValuePayload :: Parser Payload
parseValuePayload = Value <$> parseValuePayload' 0
    where parseValuePayload' p = do
            lastGroup <- (True <$ char '0') <|> (False <$ char '1')
            group <- parseBinDigit 4 p
            if lastGroup then
                return group
            else
                parseValuePayload' group

parseOperatorPayload :: Parser Payload
parseOperatorPayload = do
    lengthFlag <- (True <$ char '0') <|> (False <$ char '1')
    if lengthFlag then do
        lengthInBits <- parseBinDigit 15 0
        parserBits <- count lengthInBits anySingle
        oldInput <- getInput
        oldOffset <- getOffset
        setInput $ pack parserBits
        subPackages <- many parsePacket
        setInput oldInput
        setOffset oldOffset
        return $ Operator subPackages
    else do
        subCount <- parseBinDigit 11 0
        Operator <$> count subCount parsePacket

parsePacket :: Parser Packet
parsePacket = do
    header <- parseHeader
    payload <- case typeId header of
        4 -> parseValuePayload
        _ -> parseOperatorPayload

    return (Packet header payload)
    where parseHeader = Header <$> parseBinDigit 3 0 <*> parseBinDigit 3 0

parser :: Parser Packet
parser = do
    inputAsBin <- concatMap hexDigitToBin <$> many hexDigitChar
    setInput (pack inputAsBin)
    parsePacket <* many (char '0')

versionSum :: Packet -> Int
versionSum (Packet header (Value _)) = version header
versionSum (Packet header (Operator pkts)) = version header + sum (map versionSum pkts)

evalPacket :: Packet -> Int
evalPacket (Packet header (Value x)) = x
evalPacket (Packet header (Operator pkts)) = op pkts
    where 
        op = case typeId header of
            0 -> all sum
            1 -> all product
            2 -> all minimum
            3 -> all maximum
            5 -> binary (>)
            6 -> binary (<)
            7 -> binary (==)
            _ -> error ("Invalid Operation" ++ show header)

        all op = op . map evalPacket
        binary op [a, b] = if evalPacket a `op` evalPacket b then 1 else 0
        binary op _ = error "Invalid Subpacket count"


solver :: Packet -> IO ()
solver packet = do
  putStrLn ">> Part 1"
  print $ versionSum packet
  putStrLn ">> Part 2"
  print $ evalPacket packet

day :: Day
day = dayParse parser solver