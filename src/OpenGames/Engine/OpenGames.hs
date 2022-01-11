{-# LANGUAGE TypeOperators, DataKinds, GADTs, ConstraintKinds #-}

module OpenGames.Engine.OpenGames where

import Prelude hiding ((++))

import OpenGames.Engine.OpticClass
import OpenGames.Engine.TLL

-- type OpenGame :: (o :: * -> * -> * -> * -> *) (c :: * -> * -> * -> * -> *) (a :: [*]) (b :: [*]) (x :: *) (s :: *) (y :: *) (r :: *)
data OpenGame o c a b x s y r = OpenGame {
  play :: List a -> o x s y r,
  evaluate :: List a -> c x s y r -> List b
}

lift :: o x s y r -> OpenGame o c '[] '[] x s y r
lift o = OpenGame {
  play = \Nil -> o,
  evaluate = \Nil _ -> Nil
}

reindex :: (List a -> List a') -> (List a -> List b' -> List b)
        -> OpenGame o c a' b' x s y r -> OpenGame o c a b x s y r
reindex v u g = OpenGame {
  play = \a -> play g (v a),
  evaluate = \a c -> u a (evaluate g (v a) c)
}

(>>>) :: (Optic ctx1 o, Context ctx1 ctx2 o c, Unappend a, Unappend b)
      => OpenGame o c a b x s y r -> OpenGame o c a' b' y r z q
      -> OpenGame o c (a ++ a') (b ++ b') x s z q
(>>>) g h = OpenGame {
  play = \as -> case unappend as of (a, a') -> play g a >>>> play h a',
  evaluate = \as c -> case unappend as of (a, a') -> evaluate g a (cmap identity (play h a') c)
                                                  ++ evaluate h a' (cmap (play g a) identity c)
}

(&&&) :: (Optic ctx1 o, Context ctx1 ctx2 o c, Unappend a, Unappend b, ctx2 x, ctx2 x')
      => OpenGame o c a b x s y r -> OpenGame o c a' b' x' s' y' r'
      -> OpenGame o c (a ++ a') (b ++ b') (x, x') (s, s') (y, y') (r, r')
(&&&) g h = OpenGame {
  play = \as -> case unappend as of (a, a') -> play g a &&&& play h a',
  evaluate = \as c -> case unappend as of (a, a') -> evaluate g a (play h a' \\ c)
                                                  ++ evaluate h a' (play g a // c)
}
