module Utils where

    
    import Control.Monad
    import System.Random

    permutation :: StdGen -> [a] -> ([a],StdGen)
    permutation gen xs = permutation' gen xs []

    permutation' :: StdGen -> [a] -> [a] -> ([a],StdGen)
    permutation' gen [] ys = (ys,gen)
    permutation' gen xs ys = 
                let (randInt,newGen) = random gen
                    index = randInt `mod` length xs
                    nxs = take index xs ++ drop (index+1) xs
                    nys = (xs !! index):ys
                in permutation' newGen nxs nys
