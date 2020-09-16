module FelixParser where

import FelixSyntax

import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), (<|>))
import Text.Parser.Combinators
import Text.Parser.Char
import qualified Text.Parser.Expression as Ex
import qualified Text.Parser.Token as Tok
import Text.Parsec (parse, ParseError)
import Text.Parsec.Text (Parser)
import Data.Text (Text)

-- | Operator precedence table for FELIX
--  function calls > arithmetic operations > comparison operations
--  > logical operators
oprPrecedence :: Ex.OperatorTable Parser Expression
oprPrecedence =
    [[ Ex.Postfix (flip Apply <$> Tok.parens (Tok.commaSep exprParser))],
     [ infixop MUL "*" Ex.AssocLeft, infixop DIV "/" Ex.AssocLeft],
     [ infixop ADD "+" Ex.AssocLeft, infixop SUB "-" Ex.AssocLeft],
     [ infixop EQL "==" Ex.AssocNone, infixop NEQ "!=" Ex.AssocNone,
       infixop LTT "<" Ex.AssocNone , infixop LE "<=" Ex.AssocNone ,
       infixop GTT ">" Ex.AssocNone , infixop GE ">=" Ex.AssocNone
       ],
     [ infixop AND "&&" Ex.AssocNone ],
     [ infixop OR "||" Ex.AssocNone ]
    ]
    where
        infixop typeconstr symb assoc = Ex.Infix (Op typeconstr <$ Tok.symbol symb) assoc

-- | oprParser parses operator
--  result can be one of the following :
--  function, operator, constant, identifier
oprParser :: Parser Expression
oprParser = Ex.buildExpressionParser oprPrecedence $
                try functParser
            <|> Tok.parens oprParser
            <|> Const <$> try constantParser
            <|> Id <$> identifierParser

-- | exprParser parses expressions
--  result can be one of the following:
--  function, operator, constant, identifier
exprParser :: Parser Expression
exprParser = try functParser
             <|> try oprParser
             <|> Const <$> try constantParser
             <|> Id <$> identifierParser

-- | constantParser parses constant
--  result can be one of the following:
--  double, bool, string literal
constantParser :: Parser Constant
constantParser = ConstDbl <$> constDbl
                 <|> ConstBool <$> constBool
                 <|> ConstStr <$> Tok.stringLiteral
    where
        constDbl = castIntoDouble <$> Tok.integerOrDouble
        castIntoDouble (Left n) = fromInteger n
        castIntoDouble (Right n) = n
        constBool = True <$ Tok.symbol "True"
                   <|> False <$ Tok.symbol "False"

-- | Parses idenrifiers .
--  Identifiers start with lower case letter followed by any character
identifierParser :: Parser ID
identifierParser = GetID <$> ((:) <$> idThis <*> idRem) <* Tok.whiteSpace
    where
        idThis = lower
                  <|> char '_'
        idRem = many (alphaNum <|> oneOf "_'?!")

-- | statementParser parses statenent.
--  Statements can be a var,if,while,return or an expression
statementParser :: Parser Statement
statementParser = varParser
              <|> ifParser
              <|> whileParser
              <|> returnParser
              <|> try assignParser
              <|> Expr <$> exprParser <* Tok.symbol ";"
    where
        -- var id = expression;
        varParser = Var <$>
                    (Tok.symbol "var" *> identifierParser <* Tok.symbol "=") <*>
                    (exprParser <* Tok.symbol ";")
        -- var if (expression) {statements} else {statements}
        ifParser = If <$>
                    (Tok.symbol "if" *> Tok.parens exprParser) <*>
                    Tok.braces felixParser <*>
                    (Tok.symbol "else" *> Tok.braces felixParser)
        -- while (expression) {statements}
        whileParser = While <$>
                    (Tok.symbol "while" *> Tok.parens exprParser) <*>
                    Tok.braces felixParser
        -- return expression;
        returnParser = Return <$> (Tok.symbol "return" *> exprParser <* Tok.symbol ";")
        -- id = expression;
        assignParser = Assign <$> identifierParser <* Tok.symbol "=" <*> exprParser <* Tok.symbol ";"

-- | functParser parses function
--  fun (args ...) {statements}
functParser :: Parser Expression
functParser = Funct <$> (Tok.symbol "fun" *> functArgs) <*> Tok.braces felixParser
    where
        functArgs = Tok.parens (Tok.commaSep identifierParser)

-- | felixParser parses statements
--  ignores the leading whitespaces and starts parsing the statements
felixParser :: Parser Statements
felixParser = Tok.whiteSpace *> many statementParser

-- | parseText parses text into statements. This is the function that has to be
--  used by other modules. This ensures the end being eof.
parseText :: Text -> Either ParseError Statements
parseText = parse (felixParser <* eof) "stdin"
