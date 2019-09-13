eqBool :: Bool -> Bool -> Bool
eqBool False False = True
eqBool True True = True
eqBool _ _ = False

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList _ [] [] = True
eqList eqa (x:xs) (y:ys) = eqa x y && eqList eqa xs ys
eqList _ _ _ = False


eqPair ::  (a -> a -> Bool) ->  (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb (a, b) (a', b') = eqa a a' && eqb b b'

class Eq' a where        -- osztaly declaracio
    eq :: a -> a -> Bool -- osztaly metodus

instance Eq' Bool where
    eq False False = True
    eq True True = True
    eq _ _ = False

instance Eq' a => Eq' [a] where
    eq [] [] = True
    eq (x:xs) (y:ys) = eq x y && eq xs ys
    eq _  _ = False

instance (Eq' a, Eq' b) => Eq' (a, b) where
    eq (a, b) (a', b') = eq a a' && eq b b' 