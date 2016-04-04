-- Von http://book.realworldhaskell.org/read/using-parsec.html#id650268
module CSVParser (parseCSV) where


import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8


csvFile = endBy line eol
line = sepBy cell cellSeperator
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
    
cellSeperator = char ','

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
            
main = do

    csvDataString <- fmap BS.Char8.unpack $ BS.readFile "test.csv"
    case parseCSV csvDataString of
        Left e -> do putStrLn "Error parsing input:"
                     print e
        Right d -> mapM_ print d
 