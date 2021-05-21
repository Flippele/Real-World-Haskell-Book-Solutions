data List a = Cons a (List a)
            | Nil
            deriving(Show)

myList = Cons 0 (Cons 1(Cons 2(Cons 3(Cons 4 Nil))))

toList :: List a -> [a]

toList (Cons x xs) = x:toList(xs)
toList Nil = []