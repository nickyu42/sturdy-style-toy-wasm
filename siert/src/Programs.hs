module Programs
( addMul
, bools
, copy
, block
, branch0
, branch1
, branchIf
, loop
) where

import Syntax
import DirectStyleInterpreter
import ConcreteInterpreter

addMul :: Bl
addMul = Bl [Const (VNum 4),
             Const (VNum 8),
             Add,
             Const (VNum (-2)),
             Mul] [TNum]

bools :: Bl
bools = Bl [Const (VNum 1),
            Const (VNum 1),
            NumEq,
            Const (VNum 0),
            Const (VNum 1),
            Lt,
            And,
            Not] [TBool]

copy :: Bl
copy = Bl [Const (VNum 99),
           Copy] [TNum, TNum]

block :: Bl
block = Bl [Const (VBool True),
            Block (Bl [Const (VBool True)] [TBool]),
            And] [TBool]

branch0 :: Bl
branch0 = Bl [Const (VNum 2),
              Br 0,
              Const (VNum (-8)),
              Add] [TNum]

branch1 :: Bl
branch1 = Bl [Block (Bl [Const (VNum 2),
                         Br 1] []),
              Const (VNum (-6))] [TNum]

branchIf :: Bl
branchIf = Bl [Block (Bl [Const (VNum 2),
                          Const (VBool True),
                          BrIf 0,
                          Const (VNum (-8)),
                          Add] [TNum]),
               Const (VBool False)] [TBool, TNum]

loop :: Bl
loop = Bl [Const (VNum 10),
           Loop (Bl [Copy,
                     Const (VNum 0),
                     NumEq,
                     BrIf 1,
                     Const (VNum (-1)),
                     Add,
                     Br 0] [TNum]),
           Nop] [TNum]
