import Code
import TypeMap

data CType = CInt 
           | CFloat 
           | CChar 
           | CPtr CType
           | CVoid
           deriving (Eq,Show)

data CIO = CIO CType
  deriving (Eq,Show)

malloc :: Int -> TypeMap Code CType CType
malloc n x = Convert ["malloc"] (CPtr x)

free :: TypeMap Code CType CType
free (CPtr _) = Convert ["free"] CVoid

toFloat :: TypeMap Code CType CType
toFloat CInt         = Convert ["float"] CFloat
toFloat CChar        = Convert ["float"] CFloat
toFloat CFloat       = Convert ["id"] CFloat
toFloat (CPtr CChar) = Convert ["atoi"] CFloat
toFloat _            = Fail

deref :: TypeMap Code CType CType
deref (CPtr x)       = Convert ["*"] x
deref _              = Fail

refer :: TypeMap Code CType CType
refer x              = Convert ["&"] (CPtr x)

main :: IO ()
main = do
  print $ return CInt >>= toFloat >>= refer >>= deref
