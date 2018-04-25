--All ultrafilters that can be defined in Haskell are principal (of the form "return something"), so this module is totally useless.

module Ultrafilter (Ultrafilter) where

data Ultrafilter a = Ultra ((a -> Bool) -> Bool)

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




