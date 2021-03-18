module Parser.Uart
  ( uart
  ) where

import           Gasp.Function      (FuncName (..))
import           Gasp.Uart
import           Lexer
import           Parser.Common
import           Parser.Gpio        (pin)
import           Text.Parsec        (many, spaces, (<|>))
import           Text.Parsec.String (Parser)

reader :: Parser UartReader
reader = do
  reserved reservedNameUartRead
  n <- identifier
  v <- decimal
  spaces
  rfn <- FuncName <$> identifier
  pfn <- FuncName <$> identifier
  return UartReader
    { uartRName = n
    , uartRBufLen = fromIntegral v
    , uartRFn = rfn
    , uartRPFn = pfn
    }


writer :: Parser UartWriter
writer = do
  reserved reservedNameUartWrite
  n <- identifier
  cmd <- stringLiteral
  mode <- fromIntegral <$> option 0 decimal
  spaces
  return $ UartWriter n cmd 0 mode

data ReadWrite
  = Read UartReader
  | Write UartWriter


readWrite :: Parser ReadWrite
readWrite = (Read <$> reader) <|> (Write <$> writer)


bindLink :: Parser ModeBind
bindLink = do
  _ <- symbol "link"
  n <- AttrName <$> identifier
  return $ LinkAttr n

bindParser :: Parser ModeBind
bindParser = do
  _ <- symbol "->"
  bindLink

uart :: Parser Uart
uart = do
  reserved reservedNameUart
  n <- UartName <$> identifier
  rx <- pin
  tx <- pin
  speed <- fromIntegral <$> decimal
  spaces
  rws <- gaspClosure $ many readWrite

  b <- option NoBind bindParser

  return Uart
    { uartName = n
    , uartTxPin = tx
    , uartRxPin = rx
    , uartReaders = [x | Read x <- rws]
    , uartWriters = fillId 0 [x | Write x <- rws]
    , uartSpeed = speed
    , uartBind = b
    }
