import qualified Text.Printf as Text (printf)

main :: IO ()
main = do
    let tree1 = (Node 0
                    (Node 1
                        Nil
                        Nil)
                    (Node 0
                        (Node 1
                            (Node 1
                                Nil
                                Nil)
                            (Node 1
                                Nil
                                Nil))
                        (Node 0
                            Nil
                            Nil)))

    Text.printf "%s -> %s\n\n" (show tree1) (show $ countUnivalBinaryTrees tree1)

    let tree2 = (Node 1
                    (Node 1
                        (Node 1
                            Nil
                            Nil)
                        (Node 1
                            Nil
                            Nil))
                    (Node 1
                        (Node 1
                            Nil
                            Nil)
                        (Node 1
                            Nil
                            Nil)))

    Text.printf "%s -> %s\n\n" (show tree2) (show $ countUnivalBinaryTrees tree2)

    let tree3 = (Node 1
                    (Node 1
                        (Node 1
                            Nil
                            Nil)
                        Nil)
                    (Node 1
                        (Node 1
                            Nil
                            Nil)
                        (Node 1
                            Nil
                            Nil)))

    Text.printf "%s -> %s\n\n" (show tree3) (show $ countUnivalBinaryTrees tree3)

data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a)

instance (Show a) => Show (BinaryTree a) where
   show binaryTree = showBinaryTree binaryTree 0
       where showBinaryTree binaryTree numberOfTabs = case binaryTree of
                Nil -> "Nil"
                (Node value left right) -> "(Node " ++ show value ++ "\n" ++
                    (addTabs (numberOfTabs + 1)) ++ (showBinaryTree left (numberOfTabs + 2)) ++ "\n" ++
                    (addTabs (numberOfTabs + 1)) ++ (showBinaryTree right (numberOfTabs + 2)) ++ "\n" ++
                    (addTabs numberOfTabs) ++ ")"
                    where addTabs numberOfTabs =
                            if numberOfTabs > 0
                            then "\t" ++ addTabs (numberOfTabs - 1)
                            else ""

countUnivalBinaryTrees :: Eq a => BinaryTree a -> Int
countUnivalBinaryTrees binaryTree = case binaryTree of
    Nil -> 0
    (Node _ Nil Nil) -> 1
    (Node _ Nil right) -> 1 + countUnivalBinaryTrees right
    (Node _ left Nil) -> 1 + countUnivalBinaryTrees left
    (Node _ (Node valueLeft leftLeft leftRight) (Node valueRight rightLeft rightRight))
        | valueLeft == valueRight -> 1 + leftCount + rightCount
        | otherwise -> leftCount + rightCount
            where leftCount = countUnivalBinaryTrees (Node valueLeft leftLeft leftRight)
                  rightCount = countUnivalBinaryTrees (Node valueRight rightLeft rightRight)
