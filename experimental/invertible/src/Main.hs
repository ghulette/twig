import Term
import Expr
import Parser

readTerm :: IO Term
readTerm = do
  putStrLn "Term: "
  s <- getLine
  case parseTerm s of
    Left err -> error (show err)
    Right t -> return t

readExpr :: IO Expr
readExpr = do
  putStrLn "Rule: "
  s <- getLine
  case parseStrategy s of
    Left err -> error (show err)
    Right e -> return e

main :: IO ()
main = do
  expr <- readExpr
  t <- readTerm
  case eval expr t of
    Nothing -> putStrLn "Failed"
    Just t' -> do
      print t'
      case eval (Inv expr) t' of
        Nothing -> putStrLn "Failed"
        Just t'' -> print t''
