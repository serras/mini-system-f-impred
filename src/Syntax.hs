{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses, PatternSynonyms, ViewPatterns #-}
module Syntax where

import Data.List (intercalate)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless

type TyVar = Name Ty
data Ty = TyVar    TyVar
        | TyForAll (Bind TyVar Ty)
        | TyCon    String [Ty]
        deriving (Show, Generic, Typeable)

prettyTy :: Ty -> String
prettyTy = runFreshM . prettyTy_

prettyTy_ :: Fresh m => Ty -> m String
prettyTy_ (TyVar v)     = return $ show v
prettyTy_ (TyForAll b)  = do (x, r) <- unbind b
                             pr <- prettyTy_ r
                             return $ "{" ++ show x ++ "} " ++ pr
prettyTy_ (TyArrow s r) = do ps <- prettyTy_ s
                             pr <- prettyTy_ r
                             return $ mparens ps ++ " -> " ++ pr
prettyTy_ (TyCon c as)  = do pas <- mapM prettyTy_ as
                             return $ intercalate " " (('\'':c) : map mparens pas)

tyArrow :: Ty -> Ty -> Ty
tyArrow s t = TyCon "(->)" [s, t]

pattern TyArrow s t = TyCon "(->)" [s, t]

instance Alpha Ty
instance Subst Ty Ty where
  isvar (TyVar v) = Just (SubstName v)
  isvar _         = Nothing

type Var  = Name Expr
data Expr = Var    Var
          | App    Expr Expr
          | AppTy  Expr Ty
          | Lam    (Bind Var Expr)
          | AnnLam (Bind (Var, Embed Ty) Expr)
          deriving (Show, Generic, Typeable)

prettyExpr :: Expr -> String
prettyExpr = runFreshM . prettyExpr_

prettyExpr_ :: Fresh m => Expr -> m String
prettyExpr_ (Var v) = return $ show v
prettyExpr_ (App e1 e2) = do pe1 <- prettyExpr_ e1
                             pe2 <- prettyExpr_ e2
                             return $ pe1 ++ " " ++ mparens pe2
prettyExpr_ (AppTy e t) = do pe <- prettyExpr_ e
                             pt <- prettyTy_ t
                             return $ pe ++ " @" ++ mparens pt
prettyExpr_ (Lam b)   = do (x, e) <- unbind b
                           pe <- prettyExpr_ e
                           return $ "\\" ++ show x ++ " -> " ++ pe
prettyExpr_ (AnnLam b) = do ((x, unembed -> t), e) <- unbind b
                            pt <- prettyTy_ t
                            pe <- prettyExpr_ e
                            return $ "\\(" ++ show x ++ " :: " ++ pt ++ ") -> " ++ pe

instance Alpha Expr
instance Subst Expr Expr where
  isvar (Var v) = Just (SubstName v)
  isvar _       = Nothing
instance Subst Expr Ty where
  isvar _ = Nothing
instance Subst Ty Expr where
  isvar _ = Nothing

type Program = [Decl]
data Decl = Untyped Var Expr
          | Typed   Var Ty Expr
          | Import  Var Ty
          deriving (Show, Generic, Typeable)

prettyDecl :: Decl -> String
prettyDecl (Untyped v e) = show v ++ " = " ++ prettyExpr e
prettyDecl (Typed v t e) = show v ++ " :: " ++ prettyTy t ++ " = " ++ prettyExpr e
prettyDecl (Import v t)  = "import " ++ show v ++ " :: " ++ prettyTy t

isImport :: Decl -> Bool
isImport (Import _ _) = True
isImport _            = False

mapExpr :: (Expr -> Expr) -> Decl -> Decl
mapExpr f (Untyped v e)  = Untyped v (f e)
mapExpr f (Typed v t e)  = Typed v t (f e)
mapExpr _ i@(Import _ _) = i

type Constraint = (Ty, Ty)
type ConstraintSet = [Constraint]

mparens :: String -> String
mparens s | ' ' `elem` s = "(" ++ s ++ ")"
          | otherwise    = s
