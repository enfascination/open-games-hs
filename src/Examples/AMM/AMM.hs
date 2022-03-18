module Examples.AMM.AMM where

import Control.Monad.State

type AMM = State (A, B, LP)

type A = Int -- asset 1
type B = Int -- asset 2
type LP = Int -- liquidity pool

feeRatio :: Int
feeRatio = 100 -- we take 1/this as a fee for swapping

initialise :: (Int, Int) -> AMM ()
initialise (a, b) = put (a, b, a + b)

deposit :: (A, B) -> AMM (Bool, LP)
deposit (a', b') = do (a, b, lt) <- get
                      if a*b' == a'*b
                        then let lt' = a' + b'
                              in do put (a + a', b + b', lt + lt')
                                    return (True, lt') -- TODO: check how liquidity share is calculated
                        else return (False, 0)

withdraw :: LP -> AMM (A, B)
withdraw lt' = do (a, b, lt) <- get
                  if lt' <= lt
                     then let a' = lt'*a `div` lt
                              b' = lt'*b `div` lt
                           in do put (a - a', b - b', lt - lt')
                                 return (a', b')
                     else error "you are a magician"

swap :: Either A B -> AMM (Either A B)
swap input = do (a, b, lt) <- get
                let ab = a*b
                case input of
                  Left a' ->  do let a'' = a' - (a' `div` feeRatio) -- todo make this round in our favour
                                 let b' = b - ab `div` (a + a'')
                                 put (a + a', b - b', lt)
                                 return (Right  b')
                  Right b' -> do let b'' = b' - (b' `div` feeRatio) -- todo also here
                                 let a' = a - ab `div` (b + b'')
                                 put (a - a', b + b', lt)
                                 return (Left a')
