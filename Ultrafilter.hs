--All ultrafilters that can be defined in Haskell are principal (of the form "return something"),
--so this module is totally useless.

module Ultrafilter
            ( Ultrafilter
            , measure
            ) where

--For Ultra u to be an ultrafilter, u must have the following property:
--  For every finite list [p_0, p_1, ... p_k] :: [a -> Bool] with the property that, for every x :: a,
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

instance (Num a) => Num (Ultrafilter a) where{
  fromInteger = return.fromInteger
; (Ultra u) + (Ultra v) = Ultra $ \a -> u $ \x -> v $ \y -> a $ x+y
; (Ultra u) * (Ultra v) = Ultra $ \a -> u $ \x -> v $ \y -> a $ x*y
; abs = fmap abs
; signum = fmap signum
; negate = fmap negate
}

instance (Fractional a) => Fractional (Ultrafilter a) where{
  fromRational = return.fromRational
; recip = fmap recip
}

instance (Floating a) => Floating (Ultrafilter a) where{
  pi = return pi
; exp = fmap exp
; log = fmap log
; sin = fmap sin
; cos = fmap cos
; asin = fmap asin
; acos = fmap acos
; atan = fmap atan
; sinh = fmap sinh
; cosh = fmap cosh
; asinh = fmap asinh
; acosh = fmap acosh
; atanh = fmap atanh
}

instance (Eq a) => Eq (Ultrafilter a) where{
  u==v = measure (u >>= \x -> v >>= \y -> return $ x==y) id
}
