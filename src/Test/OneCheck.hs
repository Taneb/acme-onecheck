module Test.OneCheck where

import Data.List (intercalate)

class Arbitrary a where
  arb :: a

instance Arbitrary () where
  arb = ()

instance Arbitrary Bool where
  arb = True

instance Arbitrary Int where
  arb = 62

instance Arbitrary Integer where
  arb = 1856

instance Arbitrary Float where
  arb = 12.36

instance Arbitrary Double where
  arb = 6.15

instance Arbitrary Char where
  arb = 't'

instance Arbitrary a => Arbitrary [a] where
  arb = replicate 3 arb

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arb = (arb, arb)
{-
class CoArbitrary a where
  coarb :: a -> ()

instance CoArbitrary a where
  coarb = const ()

instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arb a = case coarb a of
    () -> arb
-}
class Testable p where
  test :: p -> Maybe [String]

instance Testable Bool where
  test False = Just []
  test True = Nothing

instance (Show a, Arbitrary a, Testable t) => Testable (a -> t) where
  test f =
    let a = arb `asArgTo` f
    in case test (f arb) of
      Nothing -> Nothing
      Just fs -> Just (show a : fs)
    where
      asArgTo :: a -> (a -> b) -> a
      asArgTo = const

onecheck :: Testable t => t -> IO ()
onecheck prop = case test prop of
  Nothing -> putStrLn "Passed 1 check(s)"
  Just fs -> case filter (not.null) fs of
    [] -> putStrLn "Failed"
    gs -> putStrLn $ "Failed with " ++ intercalate ", " gs
