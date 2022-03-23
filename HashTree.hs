-- jw418479
module HashTree where
import Hashable32
import Data.Maybe
import Utils (fromEither)
import Data.Either

data Tree a = Leaf a | Twig Hash (Tree a) | Node Hash (Tree a) (Tree a)

treeHash :: Tree a -> Hash
treeHash (Leaf x) = undefined -- trees composed of one leaf do not occur in nature
treeHash (Twig h x) = h
treeHash (Node h l r) = h

instance Hashable a => Hashable (Tree a) where
    hash (Leaf x) = hash x
    hash t = treeHash t

leaf :: Hashable a => a -> Tree a
leaf = Leaf

twig :: Hashable a => Tree a -> Tree a
twig t = Twig (hash (t, t)) t

node :: Hashable a => Tree a -> Tree a -> Tree a
node l r = Node (hash (l, r)) l r

buildTree :: Hashable a => [a] -> Tree a
buildTree [] = undefined
buildTree lst = reduce $ map leaf lst where
    reduce [Leaf x] = let [t] = enpair [Leaf x] in t -- make sure one-leaf trees do not occur in nature
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


type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

showMerklePath :: MerklePath -> String
showMerklePath [] = ""
showMerklePath (h:t) = showArrow . showString (showHash $ fromEither h) $ showMerklePath t where
    showArrow = showString (if isLeft h then "<" else ">")

instance Show a => Show (MerkleProof a) where
    showsPrec p (MerkleProof e path) = 
        showParen (p>0) (showString "MerkleProof " . shows e .
            showString " " . showString (showMerklePath path))

-- TODO hide?
gatherLeaves :: Hashable a => (MerklePath -> a -> sol) -> (sol -> sol -> sol) -> Tree a -> sol
gatherLeaves make combine = visit [] where
    visit path (Leaf x) = make path x
    visit path (Twig h t) = visit (Left (hash t):path) t
    visit path (Node h l r) = visit (Left (hash r):path) l `combine` visit (Right (hash l):path) r

-- TODO using (++) as combine may end up inefficient. Use difference lists?
merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths e = gatherLeaves make (++) where
    make path x = [reverse path | hash e == hash x]

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof e = gatherLeaves make combine where
    make path x = if hash e == hash x then Just (MerkleProof e $ reverse path) else Nothing
    combine l r = if isJust l then l else r

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof e path) = h == foldr hashEither (hash e) path where
    hashEither (Right h) acc = hash (h, acc)
    hashEither (Left h) acc = hash (acc, h)
