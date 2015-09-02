{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses, PatternSynonyms, ViewPatterns #-}
module Syntax where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Unbound.Generics.LocallyNameless

type TyVar = Name Ty
data Ty = TyVar    TyVar
        | TyForAll (Bind TyVar Ty)
        | TyCon    String [Ty]
        deriving (Show, Generic, Typeable)

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

isImport :: Decl -> Bool
isImport (Import _ _) = True
isImport _            = False

instance Alpha Decl
instance Subst Expr Decl where
  isvar _ = Nothing
instance Subst Ty Decl where
  isvar _ = Nothing

type Constraint = (Ty, Ty)
type ConstraintSet = [Constraint]
