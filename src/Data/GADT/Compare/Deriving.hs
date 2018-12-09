{-# language TemplateHaskell #-}
module Data.GADT.Compare.Deriving
  ( deriveGEq
  , makeGEq
  , deriveGCompare
  , makeGCompare
  ) where

import Control.Monad
import Language.Haskell.TH
import TH.ReifySimple
import Data.GADT.Compare

deriveGEq
  :: Name
  -> Q [Dec]
deriveGEq name =
  [d|
  instance GEq $(conT name) where
    geq = $(makeGEq name)
  |]

makeGEq
  :: Name
  -> Q Exp
makeGEq name = do
  dataType <- reifyDataType name

  let
    cons = dtCons dataType

  matches <- forM cons $ \con -> do
    fieldVars <- replicateM (length $ dcFields con)
      $ (,) <$> newName "f" <*> newName "g"
    let
      (fieldVars1, fieldVars2) = unzip fieldVars
      body = case unsnoc fieldVars of
        Nothing -> [| Just Refl |]
        Just (fieldVars', (lv1, lv2)) -> do
          let
            eq = foldr
              (\(v1, v2) e -> [| $(varE v1) == $(varE v2) && $e |])
              [| $(varE lv1) == $(varE lv2) |]
              fieldVars'
          [| if $eq then Just Refl else Nothing |]

    match
      (tupP
        [ conP (dcName con) (varP <$> fieldVars1)
        , conP (dcName con) (varP <$> fieldVars2)
        ]
      )
      (normalB body)
      []

  misMatch <- match wildP (normalB [| Nothing |]) []

  x <- newName "x"
  y <- newName "y"
  return
    $ LamE [VarP x, VarP y]
    $ CaseE (TupE [VarE x, VarE y])
    $ matches ++ [misMatch]

deriveGCompare
  :: Name
  -> Q [Dec]
deriveGCompare name =
  [d|
  instance GCompare $(conT name) where
    gcompare = $(makeGCompare name)
  |]

makeGCompare
  :: Name
  -> Q Exp
makeGCompare name = do
  dataType <- reifyDataType name

  let
    cons = dtCons dataType

  matches <- forM cons $ \con -> do
    fieldVars <- replicateM (length $ dcFields con)
      $ (,) <$> newName "f" <*> newName "g"
    let
      (fieldVars1, fieldVars2) = unzip fieldVars
      body = case unsnoc fieldVars of
        Nothing -> [| GEQ |]
        Just (fieldVars', (lv1, lv2)) -> do
          let
            comp = foldr
              (\(v1, v2) e -> [| compare $(varE v1) $(varE v2) <> $e |])
              [| compare $(varE lv1) $(varE lv2) |]
              fieldVars'
          [| case $comp of
            LT -> GLT
            EQ -> GEQ
            GT -> GGT
            |]

    match
      (tupP
        [ conP (dcName con) (varP <$> fieldVars1)
        , conP (dcName con) (varP <$> fieldVars2)
        ]
      )
      (normalB body)
      []

  misMatches <- forM (take (max 0 $ length cons - 1) cons) $ \con -> do
    let
      wildps = replicate (length $ dcFields con) wildP
    sequence
      [ match
        (tupP [conP (dcName con) wildps, wildP])
        (normalB [| GLT |])
        []
      , match
        (tupP [wildP, conP (dcName con) wildps])
        (normalB [| GGT |])
        []
      ]

  x <- newName "x"
  y <- newName "y"
  return
    $ LamE [VarP x, VarP y]
    $ CaseE (TupE [VarE x, VarE y])
    $ matches ++ concat misMatches

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc (a:as) = Just $ go a as
  where
    go x [] = ([], x)
    go x (x':xs) = do
      let (ys, y) = go x' xs
      (x:ys, y)
