{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}

module TypeChecker
( check
, check'
) where

import Prelude hiding (and, not, fail, const, id, (.), drop, exp)
import qualified Prelude as P (id, (.), not)
import Control.Category
import Control.Arrow
import Control.Lens
import Control.Lens.TH
import GenericInterpreter
import Syntax

data TypeCheckerA b c = TypeCheckerA {_getf :: (b -> Either String c)}

$(makeLenses ''TypeCheckerA)

instance Category TypeCheckerA where
    id    = TypeCheckerA (\x -> Right x)
    f . g = TypeCheckerA (\x -> case (view getf g) x of
                              Right y  -> (view getf f) y
                              Left msg -> Left msg)

instance Arrow TypeCheckerA where
    arr f   = TypeCheckerA (\x -> Right (f x))
    first a = TypeCheckerA (\(x, z) -> case (view getf a) x of
                                Right y  -> Right (y, z)
                                Left msg -> Left msg)

instance ArrowChoice TypeCheckerA where
    left a = TypeCheckerA (\eth -> case eth of
                              Right x -> Right (Right x)
                              Left  y -> case (view getf a) y of
                                  Right z  -> Right (Left z)
                                  Left msg -> Left msg)

instance ArrowFail TypeCheckerA where
    fail = TypeCheckerA (\msg -> Left msg)

instance IsVal Type TypeCheckerA where
    add       = binOp TNum TNum
    mul       = add
    and       = binOp TBool TBool
    not       = proc t -> arr snd <<< first expTy -< ((t, TBool), TBool)
    numEq     = binOp TNum TBool
    lt        = numEq
    const     = arr typeOf
    br        = branch
    brIf      = proc (n, frs, tyPopped, tys) -> do
                    expTy -< (tyPopped, TBool)
                    branch -< (n, frs, tys)
    exitFrame = proc (fr, tys) -> do
                    let depthDiff = length tys - view depth fr
                    expTys -< (view (bl . rty) fr, take depthDiff tys)
                    id -< ()

binOp :: (ArrowChoice a, ArrowFail a)
         => Type -> Type -> a (Type, Type) Type
binOp inty rty = proc (t1, t2) -> do
    expTys -< ([t1, t2], [inty, inty])
    id -< rty

expTy :: (ArrowChoice a, ArrowFail a) => a (Type, Type) ()
expTy = proc (t_act, t_exp) -> if t_exp == t_act
    then id -< ()
    else fail -< "Expected " ++ show t_exp ++ " but got " ++ show t_act ++ "."

expTys :: (ArrowChoice a, ArrowFail a) => a ([Type], [Type]) ()
expTys = proc lists -> case lists of
    ([], [])         -> id -< ()
    (t1:ts1, t2:ts2) -> do expTy -< (t1, t2)
                           expTys -< (ts1, ts2)
    _                -> fail -< "Type lists did not match in length."


branch :: (ArrowChoice a, ArrowFail a) => a (Int, [Frame], [Type]) [Frame]
branch = proc (n, frs, tys) -> do
    frs2 <- drop -< (n, frs)
    case frs2 of
        []          -> fail -< "Invalid branch depth."
        fr2hd:fr2tl -> do expTys -< (view (bl . rty) fr2hd, tys)
                          id -< (branchInto fr2hd):fr2tl

check' :: Bl -> Either String [Type]
check' b = (view getf run') ([basicFrame b], [])

check :: Bl -> String
check b = case check' b of
    Right tys -> "No errors found. Return types: " ++ show tys ++ "."
    Left msg  -> msg
