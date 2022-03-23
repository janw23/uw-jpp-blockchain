-- jw418479
module HashTree where
import Hashable32

-- TODO add Twig (node with one child)
data Tree a = Leaf a | Twig Hash (Tree a) | Node Hash (Tree a) (Tree a)

-- TODO provided specification does not have "Hashable a". Is this a mistake?
treeHash :: Hashable a => Tree a -> Hash
treeHash (Leaf x) = hash x
treeHash (Twig h x) = h
treeHash (Node h l r) = h

instance Hashable a => Hashable (Tree a) where
    hash = treeHash

leaf :: Hashable a => a -> Tree a
leaf = Leaf

twig :: Hashable a => Tree a -> Tree a
twig t = Twig (hash (t, t)) t

node :: Hashable a => Tree a -> Tree a -> Tree a
node l r = Node (hash (l, r)) l r

buildTree :: Hashable a => [a] -> Tree a
buildTree [] = undefined
buildTree lst = reduce $ map leaf lst where
    reduce [t] = t
    reduce ts = reduce $ enpair ts
    enpair (l:r:t) = node l r : enpair t
    enpair [t] = [twig t]
    enpair [] = []

-- TODO make an abstraction of prefixing with tabs?
-- TODO provided specification does not have "Hashable a". Is this a mistake?
drawTree :: (Hashable a, Show a) => Tree a -> String
drawTree = draw "" where
    draw pref (Leaf x) = shwHead pref (hash x) (show x) ""
    draw pref (Node h l r) = shwHead pref h "-" . endl . shwSubTree pref l . endl . shwSubTree pref r $ ""
    draw pref (Twig h t) = shwHead pref h "+" . endl . shwSubTree pref t $ ""
    shwHead p h s = showString p . showString (showHash h) . showString (' ':s)
    shwSubTree pref t = showString $ draw ("  "++pref) t
    endl = showString "\n"