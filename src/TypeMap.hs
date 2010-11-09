{-# LANGUAGE ExistentialQuantification #-}

module TypeMap where

import Data.Monoid

data Conversion m a = Convert m a
                    | Fail
                    deriving (Eq,Show)

instance Monoid m => Functor (Conversion m) where
  fmap f (Convert m x) = Convert m (f x)
  fmap _ Fail = Fail

instance Monoid m => Monad (Conversion m) where
  (Convert m1 x1) >>= f = case f x1 of
    Convert m2 x2 -> Convert (m1 `mappend` m2) x2
    Fail -> Fail
  Fail >>= _ = Fail
  return = Convert mempty

type TypeMap m a b = a -> Conversion m b

success :: Monoid m => TypeMap m a a
success = return

failure :: Monoid m => TypeMap m a b
failure _ = Fail

test :: Monoid m => TypeMap m a b -> TypeMap m a a
test f x = case f x of
  Convert _ _ -> return x
  Fail -> Fail

neg :: Monoid m => TypeMap m a b -> TypeMap m a a
neg f x = case f x of
  Convert _ _ -> Fail
  Fail -> return x

(|||) :: TypeMap m a b -> TypeMap m a b -> TypeMap m a b
(|||) f1 f2 x = case f1 x of
  Convert m x' -> Convert m x'
  Fail -> f2 x

branch :: Monoid m => TypeMap m a b -> TypeMap m a c -> TypeMap m a (b,c)
branch f1 f2 a = case f1 a of
  Convert m1 b -> case f2 a of
    Convert m2 c -> Convert (m1 `mappend` m2) (b,c)
    Fail -> Fail
  Fail -> Fail

congr :: Monoid m => TypeMap m a b -> TypeMap m c d -> TypeMap m (a,c) (b,d)
congr f1 f2 (a,b) = case (f1 a,f2 b) of
  (Convert m1 c,Convert m2 d) -> Convert (m1 `mappend` m2) (c,d)
  otherwise -> Fail

-- val map : 'a t -> 'a t
-- val filter : 'a t list -> 'a t
-- val fold :  'a t -> ('a t -> 'a t) list -> 'a t
-- val ith : int -> 'a t
-- val permute : int list -> 'a t
