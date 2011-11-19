module Block.Lang.CParser where

import Text.Parsec

csvFile :: Parsec [Char] s [[String]]
csvFile = do 
  result <- many line
  eof
  return result

-- Each line contains 1 or more cells, separated by a comma
--line :: GenParser Char st [String]
line :: Parsec [Char] s [String]
line = do 
  result <- cells
  eol
  return result

cells :: Parsec [Char] s [String]
cells = do 
  first <- cellContent
  next <- (char ',' >> cells) <|> (return [])
  return (first : next)

cellContent :: Parsec [Char] s String
cellContent = many (noneOf ",\n")

eol :: Parsec [Char] s ()
eol = char '\n' >> return ()

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile ""

main :: IO ()
main = do
  putStrLn "Ok"
