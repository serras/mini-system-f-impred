{-# LANGUAGE FlexibleContexts #-}
module Solver where

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold

import Syntax

type InertSet = ConstraintSet

solve' :: Subst Ty e => e -> ConstraintSet -> e
solve' e cs = fst (solve e cs [])

solve :: Subst Ty e => e -> ConstraintSet -> InertSet -> (e, InertSet)
solve e [] inert = (e, inert)
solve e ((t1@(TyCon c1 a1), t2@(TyCon c2 a2)) : r) inert
  | c1 == c2, length a1 == length a2
  = solve e (r ++ zip a1 a2) inert
  | otherwise
  = error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
solve _ ((t1@(TyCon _ _), t2@(TyForAll _)) : _) _
  = error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
solve _ ((t1@(TyForAll _), t2@(TyCon _ _)) : _) _
  = error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
solve _ ((t1@(TyVar v), t2@(TyCon _ _)) : _) _
  | v `elem` fvs t2
  = error $ "Occurs check " ++ show t1 ++ " on " ++ show t2
solve e ((TyVar v, t2) : r) inert
  = solve (subst v t2 e) (map (subst v t2) (r ++ inert)) []
solve e ((t1, TyVar v) : r) inert
  = solve e ((TyVar v, t1) : r) inert
solve e (other : r) inert
  = solve e r (other : inert)

fvs :: Ty -> [TyVar]
fvs = toListOf fv
