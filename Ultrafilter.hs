--All ultrafilters that can be defined in Haskell are principal (of the form "return something"),
--so this module is totally useless.

module Ultrafilter
            ( Ultrafilter
            , measure
            ) where

--For Ultra u to be an ultrafilter, u must have the following property:
--  For every finite list [p_0, p_1, ... p_k] :: [a -> Bool] with the preperty that, for every x :: a,
--  there is exactly one i in [0...k] such that p_i x, there is exactly one i in [0...k] such that u p_i.
data Ultrafilter a = Ultra ((a -> Bool) -> Bool)

measure :: Ultrafilter a -> (a -> Bool) -> Bool
measure (Ultra u) s = u s

instance Functor Ultrafilter where{
  fmap f (Ultra u) = Ultra $ u.(.f)
}

instance Applicative Ultrafilter where{
  pure = return
; u <*> v = do f <- u
               x <- v
               return $ f x
}

instance Monad Ultrafilter where{
  return = Ultra . (flip ($))
; u >>= f = m $ f <$> u where m (Ultra v) = Ultra $ v.(\s (Ultra w) -> w s)
}

instance (Monoid m) => Monoid (Ultrafilter m) where{
  mempty = return mempty
; (Ultra u) `mappend` (Ultra v) = Ultra $ \a -> u $ \x -> v $ \y -> a $ x`mappend`y
}
