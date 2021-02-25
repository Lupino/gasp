module Parser.Command
    ( command
    ) where

import           Data.Maybe         (fromMaybe, listToMaybe)
import           Gasp.Command
import           Gasp.Flag          (initFlag)
import           Lexer
import           Parser.Common
import           Text.Parsec
import           Text.Parsec.String (Parser)
import           Text.Printf        (printf)

getFromList :: a -> [a] -> a
getFromList def = fromMaybe def . listToMaybe

data DocItemProperty = ItemDocs ![String] | Payload !String

docItemProperties :: Parser [DocItemProperty]
docItemProperties = commaSep1 $ docItemPropertyDocs <|> docItemPropertyPayload

docItemPropertyDocs :: Parser DocItemProperty
docItemPropertyDocs = ItemDocs <$> gaspProperty "docs" (gaspList stringLiteral)

docItemPropertyPayload :: Parser DocItemProperty
docItemPropertyPayload = Payload <$> gaspProperty "payload" (strip <$> manyTill anyChar (try (symbol "\n")))

-- | Top level parser, parses DocItem.
docItem :: String -> Parser DocItem
docItem def = do
  props <- gaspClosure docItemProperties

  return DocItem
    { itemDocs = concat [t | ItemDocs t <- props]
    , itemCmd  = getFromList def [t | Payload t <- props]
    }

data DocProperty = DocName !String | DocCmd !DocItem | DocRet !DocItem | DocErr !DocItem

docPropertyName :: Parser DocProperty
docPropertyName = DocName <$> gaspProperty "name" stringLiteral

docPropertyCmd :: Parser DocProperty
docPropertyCmd =  DocCmd <$> gaspProperty "command" (docItem "")

docPropertyRet :: Parser DocProperty
docPropertyRet =  DocRet <$> gaspProperty "return" (docItem "{\"result\": \"OK\"}")

docPropertyErr :: Parser DocProperty
docPropertyErr =  DocRet <$> gaspProperty "error" (docItem "")

docProperties :: Parser [DocProperty]
docProperties = commaSep1 $ docPropertyName <|> docPropertyCmd <|> docPropertyRet <|> docPropertyErr

noneDocItem :: DocItem
noneDocItem = DocItem
  { itemDocs = []
  , itemCmd = ""
  }

retDocItem :: DocItem
retDocItem = DocItem
  { itemDocs = []
  , itemCmd = "{\"result\": \"OK\"}"
  }

-- | Top level parser, parses Doc.
doc :: Parser Doc
doc = do
  props <- gaspClosure docProperties

  return Doc
    { docName = getFromList "" [t | DocName t <- props]
    , docCmd = getFromList noneDocItem [t | DocCmd t <- props]
    , docRet = getFromList retDocItem [t | DocRet t <- props]
    , docErr = getFromList noneDocItem [t | DocErr t <- props]
    }

defDoc :: Doc
defDoc = Doc
  { docName = ""
  , docCmd = noneDocItem
  , docRet = retDocItem
  , docErr = noneDocItem
  }

updateDocName :: String -> Doc -> Doc
updateDocName name d@Doc
  { docName = ""
  } = d
    { docName = "Command " ++ name
    }
updateDocName _ d = d

updateDocCmd :: String -> Doc -> Doc
updateDocCmd name d@Doc
  { docCmd = i@DocItem
    { itemCmd = ""
    }
  } = d
    { docCmd = i
      { itemCmd = "{\"method\": \"" ++ name ++ "\"}"
      }
    }
updateDocCmd _ d = d

updateDocErr :: String -> Doc -> Doc
updateDocErr err d@Doc
  { docErr = i@DocItem
    { itemCmd = ""
    }
  } = d
    { docErr = i
      { itemCmd = "{\"err\": \"" ++ err ++ "\"}"
      }
    }
updateDocErr _ d = d

-- | A type that describes supported app properties.
data CommandProperty
    = Func !String
    | ErrS !String
    | DocS !Doc
    deriving (Show, Eq)

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
cmdProperties :: Parser [CommandProperty]
cmdProperties = commaSep1 $ cmdPropertyFunc <|> cmdPropertyErrS <|> cmdPropertyDocS

cmdPropertyFunc :: Parser CommandProperty
cmdPropertyFunc = Func <$> gaspProperty "fn" identifier

cmdPropertyErrS :: Parser CommandProperty
cmdPropertyErrS = ErrS <$> gaspPropertyStringLiteral "error"

cmdPropertyDocS :: Parser CommandProperty
cmdPropertyDocS = DocS <$> gaspProperty "docs" doc

-- | Top level parser, parses Command.
command :: Parser Command
command = do
  (name, props) <- gaspElementNameAndClosureContent reservedNameCommand cmdProperties

  let func = getFromList name [t | Func t <- props]
      errS = getFromList ("call %s failed" `printf` name) [t | ErrS t <- props]
      d = updateDocErr errS
        $ updateDocCmd name
        $ updateDocName name
        $ getFromList defDoc [t | DocS t <- props]

  return Command
    { cmdName = name
    , cmdFunc = func
    , cmdFlag = initFlag func
    , cmdErrS = errS
    , cmdDocS = d
    }
