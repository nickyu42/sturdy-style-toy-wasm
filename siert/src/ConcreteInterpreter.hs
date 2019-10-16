{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}

module ConcreteInterpreter
( run
) where

import Prelude hiding (and, not, fail, const, id, (.), drop)
import qualified Prelude as P (id, (.), not)
import Control.Category
import Control.Arrow
import Control.Lens
import Control.Lens.TH
import GenericInterpreter
import Syntax

data ConcreteA b c = ConcreteA {_getf :: (b -> c)}

$(makeLenses ''ConcreteA)

instance Category ConcreteA where
    id    = ConcreteA P.id
    f . g = ConcreteA ((view getf f) P.. (view getf g))

instance Arrow ConcreteA where
    arr f   = ConcreteA f
    first a = ConcreteA (\(x, y) -> ((view getf a) x, y))

instance ArrowChoice ConcreteA where
    left a = ConcreteA (\x -> case x of
        Left y  -> Left ((view getf a) y)
        Right z -> Right z)

instance ArrowFail ConcreteA where
    fail = proc str -> id -< error str

instance IsVal Value ConcreteA where
    add       = binOp getNum (\x y -> VNum (x + y))
    mul       = binOp getNum (\x y -> VNum (x * y))
    and       = binOp getBool (\a b -> VBool (a && b))
    not       = getBool >>> arr (\b -> VBool (P.not b))
    numEq     = binOp getNum (\x y -> VBool (x == y))
    lt        = binOp getNum (\x y -> VBool (y < x))
    const     = id
    br        = proc (n, frs, _) -> branch -< (n, frs)
    brIf      = proc (n, frs, v, _) -> do cond <- getBool -< v
                                          if cond then branch -< (n, frs)
                                          else id -< frs
    exitFrame = arr (\_ -> ())

binOp :: (ArrowChoice a, ArrowFail a)
         => a Value b -> (b -> b -> Value) -> a (Value, Value) Value
binOp getV op = proc (v1, v2) -> do
    x <- getV -< v1
    y <- getV -< v2
    id -< x `op` y

getNum :: (ArrowChoice a, ArrowFail a) => a Value Int
getNum = proc v -> case v of
    VNum n -> id -< n
    _      -> fail -< "Expected numerical value."

getBool :: (ArrowChoice a, ArrowFail a) => a Value Bool
getBool = proc v -> case v of
    VBool b -> id -< b
    _       -> fail -< "Expected boolean value."

branch :: (ArrowChoice a, ArrowFail a) => a (Int, [Frame]) [Frame]
branch = drop >>> proc frs2 -> case frs2 of
    []     -> fail -< "Invalid branch depth."
    fr:frs -> id -< (branchInto fr):frs

run :: Bl -> [Value]
run b = (view getf run') ([basicFrame b], [])
