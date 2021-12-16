module Aoc.Y2021.D16
  ( day
  ) where

import           Control.Monad

import           Aoc.Day
import           Aoc.Parse
import Numeric (readInt, showIntAtBase)
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (foldl')
import Data.Text (Text, pack)
import Data.Text.Internal.Read (hexDigitToInt)
import qualified Debug.Trace as Debug
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

parser :: Parser Packet
parser = do
    inputAsBin <- concatMap hexDigitToBin <$> many hexDigitChar
    setInput (pack inputAsBin)
    parsePacket <* many (char '0')
    where
        parsePacket = do
            header <- parseHeader
            payload <- case typeId header of
                4 -> Value <$> parseChunkedNum 0
                _ -> Operator <$> parseOperator

            return (Packet header payload)

        binDigit :: Int -> Int -> Parser Int
        binDigit n p = bin2Num p <$> count n binDigitChar
        bin2Num p = foldl' (\sum i -> sum * 2 + digitToInt i) p
        parseHeader = Header <$> binDigit 3 0 <*> binDigit 3 0

        parseChunkedNum p = do
            lastGroup <- (True <$ char '0') <|> (False <$ char '1')
            group <- binDigit 4 p
            if lastGroup then
                return group
            else
                parseChunkedNum group

        parseOperator = do
            lengthFlag <- (True <$ char '0') <|> (False <$ char '1')
            if lengthFlag then do
                lengthInBits <- binDigit 15 0
                parserBits <- count lengthInBits anySingle

                oldInput <- getInput
                oldOffset <- getOffset
                setInput $ pack parserBits

                subPackages <- many parsePacket

                setInput oldInput
                setOffset oldOffset
                return subPackages
            else do
                subCount <- binDigit 11 0
                count subCount parsePacket


versionSum :: Packet -> Int
versionSum (Packet header (Value _)) = version header
versionSum (Packet header (Operator pkts)) = version header + sum (map versionSum pkts)

evalPacket :: Packet -> Int
evalPacket (Packet header (Value x)) = x
evalPacket (Packet header (Operator pkts)) = case typeId header of
    0 -> all sum pkts
    1 -> all product pkts
    2 -> all minimum pkts
    3 -> all maximum pkts
    5 -> binary (>) pkts
    6 -> binary (<) pkts
    7 -> binary (==) pkts
    _ -> error ("Invalid Operation" ++ show header)
    where 
        all op = op . map evalPacket
        binary op pkts = if liftA2 op (evalPacket . head) (evalPacket . head . tail) pkts then 1 else 0


solver :: Packet -> IO ()
solver packet = do
  putStrLn ">> Part 1"
  print $ versionSum packet
  putStrLn ">> Part 2"
  print $ evalPacket packet

day :: Day
day = dayParse parser solver
