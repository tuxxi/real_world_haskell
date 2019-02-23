-- list type (recursive)
data List a = Cons a (List a)
            | Nil
            deriving (Show)

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

-- binary tree type
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

simpleTree :: Tree [Char]
simpleTree = Node "Parent" (Node "left" Empty Empty)
                           (Node "right"
                                        (Node "left sub" Empty Empty)
                                         Empty)


-- 1) Write converse of fromList for List: take List a and generate [a]
toList :: List a -> [a]
toList Nil              = []
toList (x `Cons` xs)    = x : toList xs

-- 2) Define tree type with one ctor using Maybe
data MTree a = MNode a (Maybe (MTree a)) (Maybe (MTree a))
             deriving (Show)

-- a leaf
sMLeaf :: a -> Maybe (MTree a)
sMLeaf a = Just (MNode a Nothing Nothing)

-- a simple test tree
sMTree :: MTree [Char]
sMTree = MNode "Parent" (sMLeaf "Left")
                        (sMLeaf "Right")