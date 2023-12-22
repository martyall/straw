{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Examples.Sudoku where

import qualified Categorifier.Categorify as Categorify
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Type.Nat (Nat3, Nat9)
import Data.Vec.Lazy (Vec (..), chunks, foldMap, fromList, tabulate, toList, (!))
import Snarkl.Language (Ty (TBool), (>>=))
import qualified Snarkl.Language as Snarkl
import Straw (Straw (Straw, runStraw))
import Prelude hiding (foldMap, fromList, repeat, zipWith, (!), (>>=))

type Sudoku = Vec Nat9 (Vec Nat9 Rational)

{-# INLINE transpose #-}
transpose :: Sudoku -> Sudoku
transpose f = tabulate (\k -> fmap (! k) f)

validateSudoku :: Sudoku -> Bool
validateSudoku g =
  all validLine g
    && all validLine (transpose g)
    && all (all validBox) (toBoxes g)
  where
    validLine :: Vec Nat9 Rational -> Bool
    validLine line = sort (toList line) == [1 .. 9]

type Box = Vec Nat3 (Vec Nat3 Rational)

toBoxes :: Sudoku -> Vec Nat3 (Vec Nat3 Box)
toBoxes g = chunks $ fmap chunks g

validBox :: Box -> Bool
validBox b = sort (numbersInBox b) == [1 .. 9]
  where
    numbersInBox :: Box -> [Rational]
    numbersInBox = foldMap (foldr (:) [])

sudokuFromList :: [[Rational]] -> Sudoku
sudokuFromList p = fromJust (fromList =<< traverse fromList p)

validPuzzule :: [[Rational]]
validPuzzule =
  [ [5, 3, 4, 6, 7, 8, 9, 1, 2],
    [6, 7, 2, 1, 9, 5, 3, 4, 8],
    [1, 9, 8, 3, 4, 2, 5, 6, 7],
    [8, 5, 9, 7, 6, 1, 4, 2, 3],
    [4, 2, 6, 8, 5, 3, 7, 9, 1],
    [7, 1, 3, 9, 2, 4, 8, 5, 6],
    [9, 6, 1, 5, 3, 7, 2, 8, 4],
    [2, 8, 7, 4, 1, 9, 6, 3, 5],
    [3, 4, 5, 2, 8, 6, 1, 7, 9]
  ]

invalidSudokuPuzzle :: [[Rational]]
invalidSudokuPuzzle =
  [ [5, 3, 4, 6, 7, 8, 9, 1, 2],
    [6, 7, 2, 1, 9, 5, 3, 4, 8],
    [1, 9, 8, 3, 4, 2, 5, 6, 7],
    [8, 5, 9, 7, 6, 1, 4, 2, 3],
    [4, 2, 6, 8, 5, 3, 7, 9, 1],
    [7, 1, 3, 9, 2, 4, 8, 5, 6],
    [9, 6, 1, 5, 3, 7, 2, 8, 4],
    [2, 8, 7, 4, 1, 9, 6, 3, 5],
    [3, 4, 5, 2, 8, 6, 5, 7, 9] -- Invalid due to two 5s in this row
  ]

sudoku :: Snarkl.Comp 'Snarkl.TBool
sudoku =
  let compiledProg :: Straw Sudoku Bool
      compiledProg = Categorify.expression validateSudoku
   in do
        input <- Snarkl.arr 81
        puzzle <- Snarkl.arr2 9 9
        _ <- Snarkl.forall2 ([0 .. 8], [0 .. 8]) $ \i j -> do
          Snarkl.get (input, i * 9 + j) >>= Snarkl.set2 (puzzle, i, j)
        runStraw compiledProg puzzle