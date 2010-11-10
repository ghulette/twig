import Code
import Typemap

data CType = CInt 
           | CFloat 
           | CChar 
           | CPtr CType
           | CVoid
           deriving (Eq,Show)

data CIO = CIO CType
  deriving (Eq,Show)

malloc :: Int -> Typemap Code CType CType
malloc n x = Convert ["malloc"] (CPtr x)

free :: Typemap Code CType CType
free (CPtr _) = Convert ["free"] CVoid

toFloat :: Typemap Code CType CType
toFloat CInt         = Convert ["float"] CFloat
toFloat CChar        = Convert ["float"] CFloat
toFloat CFloat       = Convert ["id"] CFloat
toFloat (CPtr CChar) = Convert ["atoi"] CFloat
toFloat _            = Fail

deref :: Typemap Code CType CType
deref (CPtr x)       = Convert ["*"] x
deref _              = Fail

refer :: Typemap Code CType CType
refer x              = Convert ["&"] (CPtr x)

main :: IO ()
main = do
  print $ return CInt >>= toFloat >>= refer >>= deref
