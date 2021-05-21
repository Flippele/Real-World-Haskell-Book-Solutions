data Tree a = Tree a (Maybe (Tree a)) (Maybe (Tree a))
            deriving(Show)

simpleTree = Tree "parent" (Just (Tree "left child" Nothing Nothing)) (Just (Tree "right child" Nothing Nothing))