{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Pretty where

import Data.List (intercalate)
import Unbound.Generics.LocallyNameless

import Syntax

class FreshPretty p where
  freshPretty :: Fresh m => p -> m String
instance FreshPretty Ty where
  freshPretty = prettyTy
instance FreshPretty Expr where
  freshPretty = prettyExpr
instance FreshPretty Decl where
  freshPretty = prettyDecl

pretty :: FreshPretty p => p -> String
pretty = runFreshM . freshPretty

prettyTy :: Fresh m => Ty -> m String
prettyTy (TyVar v)
  = return $ show v
prettyTy ty@(TyForAll _)
  = do (vars, r) <- peel_forall ty
       pr <- prettyTy r
       let fs = intercalate " " ("forall" : map show vars) ++ ". "
       return $ fs ++ pr
prettyTy (TyArrow s r)
  = do ps <- prettyTy s
       pr <- prettyTy r
       return $ mparens ps ++ " -> " ++ pr
prettyTy (TyCon c as)
  = do pas <- mapM prettyTy as
       return $ intercalate " " (('\'' : c) : map mparens pas)

peel_forall :: Fresh m => Ty -> m ([TyVar], Ty)
peel_forall (TyForAll b)
  = do (x, r) <- unbind b
       (vars, ty) <- peel_forall r
       return (x:vars, ty)
peel_forall ty = return ([], ty)

prettyExpr :: Fresh m => Expr -> m String
prettyExpr (Var v)
  = return $ show v
prettyExpr (App e1 e2)
  = do pe1 <- prettyExpr e1
       pe2 <- prettyExpr e2
       return $ pe1 ++ " " ++ mparens pe2
prettyExpr (AppTy e t)
  = do pe <- prettyExpr e
       pt <- prettyTy t
       return $ pe ++ " @" ++ mparens pt
prettyExpr (Lam b)
  = do (x, e) <- unbind b
       pe <- prettyExpr e
       return $ "\\" ++ show x ++ " -> " ++ pe
prettyExpr (AnnLam b)
  = do ((x, unembed -> t), e) <- unbind b
       pt <- prettyTy t
       pe <- prettyExpr e
       return $ "\\(" ++ show x ++ " :: " ++ pt ++ ") -> " ++ pe

prettyDecl :: Fresh m => Decl -> m String
prettyDecl (Import v t)
  = do pt <- prettyTy t
       return $ "import " ++ show v ++ " :: " ++ pt
prettyDecl (Typed v t e)
  = do pt <- prettyTy t
       pe <- prettyExpr e
       return $ show v ++ " :: " ++ pt ++ "\n= " ++ pe
prettyDecl (Untyped v e)
  = do pe <- prettyExpr e
       return $ show v ++ " = " ++ pe

mparens :: String -> String
mparens s | ' ' `elem` s = "(" ++ s ++ ")"
          | otherwise    = s
