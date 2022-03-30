{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Examples.AMM.AMM where

import Control.Monad.State
import GHC.Exts
import GHC.IO
import Data.IORef

type AMM = State (A, B, LP)

newtype A = A { getA :: Int }
  deriving Num via Int -- asset 1
  deriving Eq via Int
  deriving Ord via Int
  deriving Enum via Int
  deriving Real via Int
  deriving Integral via Int
  deriving Show via Int
newtype B = B { getB :: Int }
  deriving Num via Int-- asset 2
  deriving Eq via Int
  deriving Ord via Int
  deriving Enum via Int
  deriving Real via Int
  deriving Integral via Int
  deriving Show via Int
newtype LP = LP { getLiquidity :: Int }
  deriving Num via Int-- liquidity pool
  deriving Eq via Int
  deriving Ord via Int
  deriving Enum via Int
  deriving Real via Int
  deriving Integral via Int
  deriving Show via Int

infixl 8 *|*
infixl 7 +|+

-- product that forgets about the wrappers
(*|*) :: (Coercible a Int, Coercible b Int) => a -> b -> Int
a *|* b = coerce a * coerce b
-- sum that forgets about the wrappers
(+|+) :: (Coercible a Int, Coercible b Int) => a -> b -> Int
a +|+ b = coerce a + coerce b

feeRatio :: Int
feeRatio = 100 -- we take 1/this as a fee for swapping

initialise :: (Int, Int) -> AMM ()
initialise (a, b) = put (A a, B b, LP (a + b))

deposit :: (A, B) -> AMM (Bool, LP)
deposit (a', b') = do (a :: A, b :: B, lt :: LP) <- get
                      if a *|* b' == a' *|* b
                        then let lt' = LP (a' +|+ b')
                              in do put (a + a', b + b', lt + lt')
                                    return (True, lt') -- TODO: check how liquidity share is calculated
                        else return (False, 0)

withdraw :: LP -> AMM (A, B)
withdraw lt' = do (a, b, lt) <- get
                  if lt' <= lt
                     then let a' = a * coerce lt' `div` coerce lt
                              b' = b * coerce lt' `div` coerce lt
                           in do put (a - a', b - b', lt - lt')
                                 return (a', b')
                     else error "you are a magician"

swap :: Either A B -> AMM (Either A B)
swap input = do (a, b, lt) <- get
                let ab = a *|* b
                case input of
                  Left a' ->  do let a'' = a' - (a' `div` A feeRatio) -- todo make this round in our favour
                                 let b' = b - coerce ab `div` coerce (a + a'')
                                 put (a + a', b - b', lt)
                                 return (Right  b')
                  Right b' -> do let b'' = b' - (b' `div` B feeRatio) -- todo also here
                                 let a' = a - coerce ab `div` coerce (b + b'')
                                 put (a - a', b + b', lt)
                                 return (Left a')
