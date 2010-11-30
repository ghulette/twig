import Code
import Typemap
import Control.Monad

data CType = CInt 
           | CFloat 
           | CChar 
           | CPtr CType
           | CVoid
           deriving (Eq,Show)

data Alloc a = Alloc a deriving (Eq,Show)

instance Functor Alloc where
  fmap f (Alloc x) = Alloc (f x)
  
instance Monad Alloc where
  return = Alloc
  Alloc x >>= f = f x

malloc :: Int -> Typemap Code CType (Alloc CType)
malloc _ x = Convert ["malloc"] (Alloc (CPtr x))

free :: Typemap Code (Alloc CType) CType
free (Alloc (CPtr _)) = Convert ["free"] CVoid
free _ = Fail

toFloat :: Typemap Code CType CType
toFloat CInt          = Convert ["float"] CFloat
toFloat CChar         = Convert ["float"] CFloat
toFloat CFloat        = Convert ["id"] CFloat
toFloat (CPtr CChar)  = Convert ["atoi"] CFloat
toFloat _             = Fail

deref :: Typemap Code CType CType
deref (CPtr x)        = Convert ["*"] x
deref _               = Fail

refer :: Typemap Code CType CType
refer x               = Convert ["&"] (CPtr x)

convert :: Typemap Code CType CType
convert x = do
  y <- toFloat x
  z <- refer y
  deref z

convert2 :: Typemap Code CType CType
convert2 x = do
  y <- malloc 10 x
  free y

main :: IO ()
main = do
  print $ return CInt >>= convert
  print $ return CFloat >>= convert2
