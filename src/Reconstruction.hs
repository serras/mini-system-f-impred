{-# LANGUAGE FlexibleContexts, ViewPatterns #-}
module Reconstruction where

import Control.Monad.Reader
import Data.List (partition)
import Unbound.Generics.LocallyNameless

import Syntax

type Env = [(Var, Ty)]

reconstructProgram :: Program -> [(Decl, ConstraintSet)]
reconstructProgram decls
  = let (imports, rest) = partition isImport decls
        env = map (\(Import v t) -> (v, t)) imports
     in flip runReader env $ runFreshMT (mapM reconstructDecl rest)

reconstructDecl :: (MonadReader Env m, Fresh m)
               => Decl -> m (Decl, ConstraintSet)
reconstructDecl (Typed v ty e)
  = do (e', cs) <- reconstruct e ty
       return (Typed v ty e', cs)
reconstructDecl (Untyped v e)
  = do ty <- TyVar <$> fresh (s2n "ty")
       (e', cs) <- reconstruct e ty
       return (Typed v ty e', cs)
reconstructDecl (Import _ _)
  = error "Imports should not be here!"

reconstruct :: (MonadReader Env m, Fresh m)
            => Expr -> Ty -> m (Expr, ConstraintSet)
reconstruct e (TyForAll b)
  = do (_, r) <- unbind b
       reconstruct e r
reconstruct e ty
  = do r <- reconstructUp e
       case r of
         Just (e', tau, cs) -> return (e', doUp tau ty cs)
         Nothing            -> doRest e ty

doUp :: Ty -> Ty -> ConstraintSet -> ConstraintSet
-- doUp _   (TyVar _)       cs = cs
doUp tau ups@(TyVar _)   cs = (tau, ups) : cs
doUp tau psi@(TyCon _ _) cs = (tau, psi) : cs
doUp _   (TyForAll _)    _  = error "This should never happen!"

doRest :: (MonadReader Env m, Fresh m)
       => Expr -> Ty -> m (Expr, ConstraintSet)
doRest _ (TyForAll _)
  = error "A forall should never be there!"
doRest (AppTy _ _) _
  = error "Reconstruction does not support type app"
doRest (Var v) _
  = error $ "Not existing variable " ++ show v
doRest (Lam b) (TyVar _)
  = do (x, e) <- unbind b
       alpha <- TyVar <$> fresh (s2n "a")
       beta  <- TyVar <$> fresh (s2n "b")
       (e', cs) <- local ((x, alpha) : ) $ reconstruct e beta
       return (Lam (bind x e'), cs)
doRest (Lam b) (TyArrow s r)
  = do (x, e) <- unbind b
       (e', cs) <- local ((x, s) : ) $ reconstruct e r
       return (Lam (bind x e'), cs)
doRest (Lam _) (TyCon _ _)
  = error "Function without an arrow type!"
doRest (AnnLam b) v@(TyVar _)
  = do ((x, unembed -> t1), e) <- unbind b
       beta  <- TyVar <$> fresh (s2n "b")
       (e', cs) <- local ((x, t1) : ) $ reconstruct e beta
       return (AnnLam (bind (x, embed t1) e'),
                 (v, tyArrow t1 beta) : cs)
doRest (AnnLam b) (TyArrow s r)
  = do ((x, unembed -> t1), e) <- unbind b
       (e', cs) <- local ((x, t1) : ) $ reconstruct e r
       return (AnnLam (bind (x, embed t1) e'), (s, t1) : cs)
doRest (AnnLam _) (TyCon _ _)
  = error "Function without an arrow type!"
doRest (App e1 e2) ty
  = do alpha <- TyVar <$> fresh (s2n "a")
       (e1', cs1) <- reconstruct e1 (tyArrow alpha ty)
       (e2', cs2) <- reconstruct e2 alpha
       return (App e1' e2', cs1 ++ cs2)

reconstructUp :: (MonadReader Env m, Fresh m)
              => Expr -> m (Maybe (Expr, Ty, ConstraintSet))
reconstructUp e@(Var v)
  = do maybeTy <- asks (lookup v)
       case maybeTy of
         Just ty -> do (e', instTy) <- instantiate e ty
                       return $ Just (e', instTy, [])
         Nothing -> return Nothing
reconstructUp e@(App _ _) | (h, args) <- apps e
  = do maybeH <- reconstructUp h
       case maybeH of
         Just (h', hTy, hCs) -> Just <$> upPhase2 h' args hTy hCs
         Nothing -> return Nothing
reconstructUp _ = return Nothing

apps :: Expr -> (Expr, [Expr])
apps (App e1 e2) = let (h, r) = apps e1
                    in (h, r ++ [e2])
apps h           = (h, [])

instantiate :: Fresh m
            => Expr -> Ty -> m (Expr, Ty)
instantiate e (TyForAll b) = do (x, r) <- unbind b
                                instantiate (AppTy e (TyVar x)) r
instantiate e ty           = return (e, ty)

upPhase2 :: (MonadReader Env m, Fresh m)
         => Expr -> [Expr] -> Ty -> ConstraintSet
         -> m (Expr, Ty, ConstraintSet)
upPhase2 e []     ty            cs = return (e, ty, cs)
upPhase2 e (a:as) (TyArrow s r) cs = do
  (a', aCs) <- reconstruct a s
  upPhase2 (App e a') as r (cs ++ aCs)
upPhase2 e as    f@(TyForAll _) cs = do
  (e', ty') <- instantiate e f
  upPhase2 e' as ty' cs
upPhase2 _ _ (TyVar _)   _ = error "Does this happen at all?"
upPhase2 _ _ (TyCon _ _) _ = error "Application without a function!"
