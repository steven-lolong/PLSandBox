import Data.Monoid
-- Monoid from prelued
-- class Monoid where 
--     mempty  :: a
--     mappend :: a -> a -> a

--     mconcat :: [a] -> a
--     mconcat = foldr mappend mempty


-- instance Monoid [a] where 
--     mempty      = []
--     mappend a b = a ++ b 
--     mconcat x   = foldr mappend empty x

data MySum a = MySum a 
    deriving (Show)

instance Num a => Monoid (MySum a) where 
    mempty          = MySum 0

instance Num a => Semigroup (MySum a) where 
    (MySum x) <> (MySum y) = MySum (x + y)

newtype MyProduct a = MyProduct {getMyProduct :: a}
    deriving (Show)

instance Num a => Semigroup (MyProduct a) where 
    (MyProduct x) <> (MyProduct y) = MyProduct (x * y)

instance Num a => Monoid (MyProduct a) where 
    mempty  = MyProduct 1

productOne :: Num a => MyProduct a 
productOne = MyProduct 5

productTwo :: Num a => MyProduct a 
productTwo = MyProduct 8

main :: IO ()
main = do 
    print $ getMyProduct productOne
    print $ getMyProduct $ productOne <> productTwo