module Strategy where

type Strategy a = a -> Maybe a

success :: Strategy a
success = Just

failure :: Strategy a
failure _ = Nothing

test :: Strategy a -> Strategy a
test s t = 
  case s t of
    Just _ -> Just t
    Nothing -> Nothing

neg :: Strategy a -> Strategy a
neg s t = 
  case s t of
    Just _ -> Nothing
    Nothing -> Just t

seqn :: Strategy a -> Strategy a -> Strategy a
seqn s1 s2 t = 
  case s1 t of
    Just t' -> s2 t
    Nothing -> Nothing

choice :: Strategy a -> Strategy a -> Strategy a
choice s1 s2 t =
  case s1 t of
    Just t' -> Just t'
    Nothing -> s2 t
