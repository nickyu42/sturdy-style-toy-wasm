{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}

module GenericInterpreter
( run'
, IsVal(..)
, ArrowFail(..)
, drop
, branchInto
) where

import Prelude hiding (and, not, fail, id, const, (.), drop)
import qualified Prelude as P ((.))
import Syntax
import Control.Lens hiding (Const)
import Control.Arrow
import Control.Category

class Arrow a => IsVal v a where
    add :: a (v, v) v
    mul :: a (v, v) v
    and :: a (v, v) v
    not :: a v v
    numEq :: a (v, v) v
    lt :: a (v, v) v
    const :: a Value v
    brIf :: a (Int, [Frame], v, [v]) [Frame]
    br :: a (Int, [Frame], [v]) [Frame]
    exitFrame :: a (Frame, [v]) ()

class Arrow a => ArrowFail a where
    fail :: a String x

run' :: (IsVal v a, ArrowChoice a, ArrowFail a)
         => a ([Frame], [v]) [v]
run' = proc (frs, vs) -> do
    (frs2, vs2) <- step -< (frs, vs)
    case frs2 of
        [] -> id -< vs2
        _  -> run' -< (frs2, vs2)

step :: (IsVal v a, ArrowChoice a, ArrowFail a)
        => a ([Frame], [v]) ([Frame], [v])
step = proc (frs, vs) -> case frs of
    []          -> id -< (frs, vs)
    (fr:frtail) -> case view (bl P.. isB) fr of
        []      -> do exitFrame -< (fr, vs)
                      id -< (frtail, vs)
        instr:_ -> case instr of
            Add       -> binOp add -< (frs', vs)
            Mul       -> binOp mul -< (frs', vs)
            And       -> binOp and -< (frs', vs)
            Not       -> second (push <<< (first not) <<< pop) -< (frs', vs)
            NumEq     -> binOp numEq -< (frs', vs)
            Lt        -> binOp lt -< (frs', vs)
            Copy      -> do (v, vs2) <- pop -< vs
                            vs3 <- push -< (v, vs2)
                            second push -< (frs', (v, vs3))
            Const val -> second (push <<< (first const)) -< (frs', (val, vs))
            Block b   -> first push -< ((BlockFrame b, frs'), vs)
            Loop b    -> first push -< ((LoopFrame b (view isB b),
                                         frs'), vs)
            Br n      -> first br -< ((n, frs', vs), vs)
            BrIf n    -> do (v, vs2) <- pop -< vs
                            first brIf -< ((n, frs', v, vs2), vs2)
            Nop       -> id -< (frs', vs)
            where frs' = (over (bl P.. isB) tail fr):frtail

pop :: (ArrowChoice a, ArrowFail a) => a [v] (v, [v])
pop = proc vs -> case vs of
    []     -> fail -< "Tried to pop from empty stack."
    (i:is) -> id -< (i, is)

pop2 :: (ArrowChoice a, ArrowFail a) => a [v] (v, v, [v])
pop2 = proc vs -> do
    (v1, vs1) <- pop -< vs
    (v2, vs2) <- pop -< vs1
    id -< (v1, v2, vs2)

push :: (ArrowChoice a) => a (v, [v]) [v]
push = proc (v, vs) -> id -< v:vs

binOp :: (ArrowChoice a, ArrowFail a) => a (v, v) v -> a (fs, [v]) (fs, [v])
binOp op = second $ proc vs -> do
    (v1, v2, vs2) <- pop2 -< vs
    res <- op -< (v1, v2)
    push -< (res, vs2)

drop :: (ArrowChoice a, ArrowFail a) => a (Int, [b]) [b]
drop = proc (n, xs) -> case (n, xs) of
    (0, xs)   -> id -< xs
    (_, [])   -> id -< []
    (n, x:xs) -> drop -< (n - 1, xs)

branchInto :: Frame -> Frame
branchInto fr = case fr of
    BlockFrame _   -> setIs []
    LoopFrame _ is -> setIs is
    where setIs is = set (bl P.. isB) is fr
