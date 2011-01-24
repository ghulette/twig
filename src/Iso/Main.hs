import Iso.Writer

type Id = String

data Type = CVoid
          | CChar
          | CPtr Type
          | CFunc Type [(Id,Type)]
          | JavaString
          | JavaMethod Type [(Id,Type)]
          deriving (Eq,Show)

type Term = (Id,Type)

type Code = [String]

type Rule = Term -> Maybe Term

-- To should be its own inverse?  But side-effects should be different for
-- inverted function?
convertTo :: Rule
convertTo (x,CPtr CChar) = Just (x,JavaString)
convertTo (x,JavaString) = Just (x,CPtr CChar)
convertTo _ = Nothing

seqn :: Rule -> Rule -> Rule 
seqn f1 f2 t = 
  case f1 t of
    Just t' -> f2 t'
    Nothing -> Nothing

choice :: Rule -> Rule -> Rule
choice f1 f2 t = 
  case f1 t of
    Just t' -> Just t'
    Nothing -> f2 t

example :: Term
example = ("foo",CFunc CVoid [("x",CPtr CChar),("y",CPtr CChar)])

main :: IO ()
main = do
  print example
  print $ convertTo example
