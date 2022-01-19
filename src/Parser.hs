module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import LispType
import qualified Data.Map as Map
import Control.Monad.Cont (void)

parseTree :: String -> String -> Either ParseError [LispData]
parseTree = parse (manyTill parseLisp eof)

parseLisp,parseDot,parseQuote,parseBool,parseList,parseSymbol,parseString,parseNumber :: Parser LispData
parseLisp = spaces *> choice [
        parseNumber,
        parseString,
        parseBool,
        parseQuote,
        parseSymbol,
        parseDot,
        parseList
    ] <* spaces
    
spaces = skipMany (char ' ' <|> char '\n' <|> char '\t' <|> (comments >> return ' '))

comments :: Parser ()
comments = char ';' >> manyTill anyToken (void (char '\n') <|> void eof) >> return ()

parseSymbol = LispSymbol <$> many1 (letter <|> choice (map char "<>=+-!?/*%"))

parseBool = LispBool . strToBool <$> (string "#t" <|> string "#f")
    where strToBool "#t" = True
          strToBool "#f" = False

parseNumber = LispNumber . read <$> many1 digit

parseString = LispString <$> (between (char '"') (char '"') $ many (noneOf "\""))

parseList = LispCons <$> between (char '(') (char ')') (many parseLisp)

parseDot = try $ LispDotList <$>
    (char '(' >>
        many parseLisp) <*>
            (char '.' >> parseLisp <* char ')')

parseQuote = LispQuote <$> (char '\'' *> (parseQuote <|> parseSymbol <|> parseList))