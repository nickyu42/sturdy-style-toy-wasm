module DirectStyleInterpreter
( run
) where

import Syntax
import Control.Lens hiding (Const)
import Control.Lens.TH

run :: Bl -> [Value]
run b = run' [basicFrame b] []

run' :: [Frame] -> [Value] -> [Value]
run' frs1 vs1 = case step frs1 vs1 of
    ([], vs2)   -> vs2
    (frs2, vs2) -> run' frs2 vs2

step :: [Frame] -> [Value] -> ([Frame], [Value])
step frs vs = case frs of
    []     -> ([], vs)
    (fr:frs) -> case view (bl . isB) fr of
        []     -> (frs, vs)
        (instr:_) -> case instr of
            Add     -> (frs', binopnum (\x y -> VNum (x + y)) vs)
            Mul     -> (frs', binopnum (\x y -> VNum (x * y)) vs)
            And     -> (frs', binopbool (\a b -> VBool (a && b)) vs)
            Not     -> (frs', (VBool (not (getBool (head vs)))):(tail vs))
            NumEq   -> (frs', binopnum (\x y -> VBool (x == y)) vs)
            Lt      -> (frs', binopnum (\x y -> VBool (y < x)) vs)
            Copy    -> (frs', (head vs):vs)
            Const v -> (frs', v:vs)
            Block b -> ((BlockFrame b (length vs)):frs', vs) -- This depth value
                                                             -- is never read.
            Loop b  -> ((LoopFrame b (length vs) (view isB b)):frs', vs)
            Br i    -> branch i frs' vs
            BrIf i  -> if (getBool (head vs)) then branch i frs' (tail vs)
                                              else (frs', tail vs)
            Nop     -> (frs', vs)
        where
            frs'              = (over (bl . isB) tail fr):frs
            getNum (VNum n)   = n
            getBool (VBool b) = b
            binop getf        = \op (v1:v2:vs) -> ((getf v1) `op` (getf v2)):vs
            binopnum          = binop getNum
            binopbool         = binop getBool
            branch i frs vs   = (resetTopFrame (drop i frs), vs)
            resetTopFrame frs = case head frs of
                fr@(BlockFrame _ _)   -> (set (bl . isB) [] fr):(tail frs)
                fr@(LoopFrame _ _ is) -> (set (bl . isB) is fr):(tail frs)
