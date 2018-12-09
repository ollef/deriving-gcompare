{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.GADT.Compare
import System.Exit

import Data.GADT.Compare.Deriving

data Q a where
  A :: Q Int
  B :: Int -> Q String
  C :: Bool -> Int -> Q Bool

deriveGEq ''Q
deriveGCompare ''Q

tests :: [Bool]
tests =
  [ geq A A == Just Refl
  , geq (B 1) (B 1) == Just Refl
  , geq (B 1) (B 2) == Nothing
  , geq (B 2) (B 1) == Nothing
  , geq (C False 0) (C False 0) == Just Refl
  , geq (C False 0) (C True 0) == Nothing
  , geq (C True 0) (C False 0) == Nothing
  , geq A (B 0) == Nothing
  , geq A (C True 0) == Nothing
  , geq (B 0) A == Nothing
  , geq (B 0) (C True 0) == Nothing
  , geq (C True 0) A == Nothing
  , geq (C False 0) (B 2) == Nothing
  , gcompare A A == GEQ
  , gcompare (B 1) (B 1) == GEQ
  , gcompare (B 1) (B 2) == GLT
  , gcompare (B 2) (B 1) == GGT
  , gcompare (C False 0) (C False 0) == GEQ
  , gcompare (C False 0) (C True 0) == GLT
  , gcompare (C False 0) (C False 1) == GLT
  , gcompare (C True 0) (C False 0) == GGT
  , gcompare (C True 1) (C True 0) == GGT
  , gcompare A (B 0) == GLT
  , gcompare A (C True 0) == GLT
  , gcompare (B 0) A == GGT
  , gcompare (B 0) (C True 0) == GLT
  , gcompare (C True 0) A == GGT
  , gcompare (C False 0) (B 2) == GGT
  ]

main :: IO ()
main = if and tests then exitSuccess else exitFailure
