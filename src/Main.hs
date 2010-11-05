import Data.Monoid

type Code = [String]

data CType = CInt 
           | CFloat 
           | CChar 
           | CPtr CType
           deriving (Eq,Show)

data Conversion m a = Converted m a
                    | Fail
                    deriving (Eq,Show)

instance Monoid m => Functor (Conversion m) where
  fmap f (Converted m x) = Converted m (f x)
  fmap _ Fail = Fail

instance Monoid m => Monad (Conversion m) where
  (Converted m1 x1) >>= f = case f x1 of
    Converted m2 x2 -> Converted (m1 `mappend` m2) x2
    Fail -> Fail
  Fail >>= _ = Fail
  return = Converted mempty

type TypeMap = CType -> Conversion Code CType

toFloat :: TypeMap
toFloat CInt         = Converted ["float"] CFloat
toFloat CChar        = Converted ["float"] CFloat
toFloat CFloat       = Converted ["id"] CFloat
toFloat (CPtr CChar) = Converted ["atoi"] CFloat
toFloat _            = Fail

deref :: TypeMap
deref (CPtr x)       = Converted ["*"] x
deref _              = Fail

refer :: TypeMap
refer x              = Converted ["&"] (CPtr x)

main :: IO ()
main = do
  print $ return CInt >>= toFloat >>= refer
