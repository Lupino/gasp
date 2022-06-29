module Parser.Uart
  ( uart
  ) where

import           Gasp.Function      (FuncName (..))
import           Gasp.Uart
import           Lexer
import           Parser.Common
import           Text.Parsec        (many, option, (<|>))
import           Text.Parsec.String (Parser)

reader :: Parser UartReader
reader = do
  reserved reservedNameUartRead
  v <- (show <$> decimal) <|> identifier
  whiteSpace
  rfn <- FuncName <$> identifier
  pfn <- FuncName <$> identifier
  on <- option "" $ block "on" "\n"
  return UartReader
    { uartRId = 0
    , uartRBufLen = v
    , uartRFn = rfn
    , uartRPFn = pfn
    , uartROn = on
    }


gen :: Parser GenOrCmd
gen = do
  fn <- FuncName <$> identifier
  v <- fromIntegral <$> decimal
  whiteSpace
  return $ Gen fn v

cmd :: Parser GenOrCmd
cmd = Cmd <$> stringLiteral


writer :: Parser UartWriter
writer = do
  reserved reservedNameUartWrite
  n <- identifier
  act <- gen <|> cmd
  on <- option "" $ block "on" "\n"
  return $ UartWriter n act 0 on

data ReadWrite
  = Read UartReader
  | Write UartWriter


readWrite :: Parser ReadWrite
readWrite = (Read <$> reader) <|> (Write <$> writer)

fillWId :: Int -> [UartWriter] -> [UartWriter]
fillWId _ []       = []
fillWId idx (x:xs)
  | uartWOn x == "false" = x : fillWId idx xs
  | otherwise = x {uartWId = idx} : fillWId (idx + 1) xs

fillRId :: Int -> [UartReader] -> [UartReader]
fillRId _ []       = []
fillRId idx (x:xs) = x {uartRId = idx} : fillRId (idx + 1) xs

uart :: Parser Uart
uart = do
  reserved reservedNameUart
  n <- UartName <$> identifier
  rws <- gaspClosure $ many readWrite

  return Uart
    { uartName    = n
    , uartReaders = fillRId 0 [x | Read x <- rws]
    , uartWriters = fillWId 0 [x | Write x <- rws]
    }
