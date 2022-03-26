-- jw418479
module HashTree where
import Hashable32
import Data.Maybe
import Utils (fromEither)
import Data.Either

data Tree a = Leaf Hash a | Twig Hash (Tree a) | Node Hash (Tree a) (Tree a)

treeHash :: Tree a -> Hash
treeHash (Leaf h x) = h
treeHash (Twig h x) = h
treeHash (Node h l r) = h

instance Hashable (Tree a) where
    hash = treeHash

leaf :: Hashable a => a -> Tree a
leaf x = Leaf (hash x) x

twig :: Hashable a => Tree a -> Tree a
twig t = Twig (hash (t, t)) t

node :: Hashable a => Tree a -> Tree a -> Tree a
node l r = Node (hash (l, r)) l r

buildTree :: Hashable a => [a] -> Tree a
buildTree [] = undefined
buildTree lst = reduce $ map leaf lst where
    reduce [t] = t
    reduce ts = reduce $ enpair ts
    enpair (l:r:ts) = node l r : enpair ts
    enpair [t] = [twig t]
    enpair [] = []

drawTree :: Show a => Tree a -> String
drawTree = draw "" where
    draw pref (Leaf h x) = shwHead pref h (show x) ""
    draw pref (Node h l r) = shwHead pref h "-" . endl . shwSubTree pref l . endl . shwSubTree pref r $ ""
    draw pref (Twig h t) = shwHead pref h "+" . endl . shwSubTree pref t $ ""
    shwHead p h s = showString p . showString (showHash h) . showString (' ':s)
    shwSubTree pref t = showString $ draw ("  "++pref) t
    endl = showString "\n"


type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

showMerklePath :: MerklePath -> String
showMerklePath [] = ""
showMerklePath (x:xs) = showArrow . showString (showHash $ fromEither x) $ showMerklePath xs where
    showArrow = showString (if isLeft x then "<" else ">")

instance Show a => Show (MerkleProof a) where
    showsPrec p (MerkleProof e path) =
        showParen (p>0) (showString "MerkleProof " . showsPrec 11 e .
            showString " " . showString (showMerklePath path))

-- Traverse the tree and combine partial solutions made when visiting each leaf.
gatherLeaves :: (MerklePath -> Hash -> sol) -> (sol -> sol -> sol) -> Tree a -> sol
gatherLeaves make combine = visit [] where
    visit path (Leaf h x) = make path h
    visit path (Twig h t) = visit (Left (hash t):path) t
    visit path (Node h l r) = visit (Left (hash r):path) l `combine` visit (Right (hash l):path) r

-- This function uses difference lists for efficient concatenation.
merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths e t = gatherLeaves make (.) t [] where
    make path h = if hash e == h then (reverse path :) else id

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof e = gatherLeaves make combine where
    make path h = if hash e == h then Just (MerkleProof e $ reverse path) else Nothing
    combine l r = if isJust l then l else r

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof e path) = h == foldr hashEither (hash e) path where
    hashEither (Right h) acc = hash (h, acc)
    hashEither (Left h) acc = hash (acc, h)