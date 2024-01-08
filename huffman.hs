import Data.List (sortBy, insertBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

data HuffmanTree a = Leaf a | Branch (HuffmanTree a) (HuffmanTree a)
    deriving Show

frequencyTable :: Ord a => [a] -> Map.Map a Int
frequencyTable = Map.fromListWith (+) . flip zip (repeat 1)

buildHuffmanTree :: Ord a => Map.Map a Int -> HuffmanTree a
buildHuffmanTree = buildTree . sortBy (comparing snd) . map (\(x, y) -> (Leaf x, y)) . Map.toList
    where
        buildTree [x] = fst x
        buildTree (x:y:xs) = buildTree $ insertBy (comparing snd) (Branch (fst x) (fst y), snd x + snd y) xs

huffmanCoding :: Ord a => HuffmanTree a -> Map.Map a [Bool]
huffmanCoding tree = codingMap tree []
    where
        codingMap (Leaf x) code = Map.singleton x code
        codingMap (Branch l r) code = Map.union (codingMap l (code ++ [False])) (codingMap r (code ++ [True]))

encode :: (Ord a) => HuffmanTree a -> [a] -> [Bool]
encode tree = concatMap (\x -> coding Map.! x) 
    where coding = huffmanCoding tree

decode :: HuffmanTree a -> [Bool] -> [a]
decode tree = decode' tree tree
    where
        decode' _ _ [] = []
        decode' (Leaf x) t bs = x : decode' t t bs
        decode' (Branch l r) t (b:bs) = decode' (if b then r else l) t bs

example :: String
example = "this is an example of a huffman tree"

freqTable = frequencyTable example

huffmanTree = buildHuffmanTree freqTable

encoded = encode huffmanTree example

decoded = decode huffmanTree encoded
