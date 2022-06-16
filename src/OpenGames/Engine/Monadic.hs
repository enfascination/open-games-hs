module OpenGames.Engine.Monadic

data MLens s r y = MkMLens {
  val :: y
  con :: (y -> r) -> Bool

Functor (MLens s t) where
  map f (MkMLens a c) = MkMLens (f a) (c . (. f))

Applicative (MLens s r) where
  pure x = MkMLens x (const True)
  f <*> x = -- MkMLens (f.a x.a) (\y => f.c (y . ($ x.a)))
            MkMLens (f.a x.a) (\y => x.c (y . f.a))
            -- Which one is it???

Monad (MLens s r) where
  m >>= f = MkMLens (f m.a).a ((f m.a).c)

record TLens (f : Type -> Type) (s, r, y : Type) where
  constructor MkTLens
  v : f y
  g : (f y -> f r) -> Bool

Functor f => Functor (TLens f s r) where
  map f (MkTLens v g) = MkTLens (map f v) (g . (. map f))

Applicative f => Applicative (TLens f s r) where
  pure x = MkTLens (pure x) (const True)
  MkTLens fv fg <*> MkTLens xv xg = MkTLens (fv <*> xv) (\n => fg (\fa => n (fa <*> xv)))

interface ExtractBool (0 m : Type -> Type) where
  extractB : m Bool -> Bool

ExtractBool f => Monad f => Monad (TLens f s r) where
  m >>= f = MkTLens (m.v >>= v . f) (\k => extractB $ do val <- m.v
                                                         let fn = (f val).g
                                                         pure (fn k))




