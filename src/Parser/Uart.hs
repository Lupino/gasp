module Parser.Uart
  ( uart
  ) where

import           Gasp.Function      (FuncName (..))
import           Gasp.Uart
import           Lexer
import           Parser.Common
import           Parser.Gpio        (pin)
import           Text.Parsec        (many, option, spaces, (<|>))
import           Text.Parsec.String (Parser)

reader :: Parser UartReader
reader = do
  reserved reservedNameUartRead
  v <- decimal
  spaces
  rfn <- FuncName <$> identifier
  pfn <- FuncName <$> identifier
  on <- option "" $ block "on" "\n"
  return UartReader
    { uartRId = 0
    , uartRBufLen = fromIntegral v
    , uartRFn = rfn
    , uartRPFn = pfn
    , uartROn = on
    }


writer :: Parser UartWriter
writer = do
  reserved reservedNameUartWrite
  n <- identifier
  cmd <- stringLiteral
  on <- option "" $ block "on" "\n"
  return $ UartWriter n cmd 0 on

data ReadWrite
  = Read UartReader
  | Write UartWriter


readWrite :: Parser ReadWrite
readWrite = (Read <$> reader) <|> (Write <$> writer)

fillWId :: Int -> [UartWriter] -> [UartWriter]
fillWId _ []       = []
fillWId idx (x:xs) = x {uartWId = idx} : fillWId (idx + 1) xs

fillRId :: Int -> [UartReader] -> [UartReader]
fillRId _ []       = []
fillRId idx (x:xs) = x {uartRId = idx} : fillRId (idx + 1) xs

uart :: Parser Uart
uart = do
  reserved reservedNameUart
  n <- UartName <$> identifier
  rx <- pin
  tx <- pin
  speed <- fromIntegral <$> decimal
  spaces
  rws <- gaspClosure $ many readWrite

  return Uart
    { uartName = n
    , uartTxPin = tx
    , uartRxPin = rx
    , uartReaders = fillRId 0 [x | Read x <- rws]
    , uartWriters = fillWId 0 [x | Write x <- rws]
    , uartSpeed = speed
    }
