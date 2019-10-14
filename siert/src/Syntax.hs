{-# LANGUAGE TemplateHaskell #-}

module Syntax
( Instr(..)
, Bl(..)
, Frame(..)
, Value(..)
, Type(..)
, isB
, rty
, bl
, allIs
, typeOf
) where

import Control.Lens hiding (Const)
import Control.Lens.TH

data Instr
    = Add
    | Mul
    | And
    | Not
    | NumEq
    | Lt
    | Copy
    | Const Value
    | Block Bl
    | Loop Bl
    | Br Int
    | BrIf Int
    | Nop
    deriving (Show, Eq)

data Value
    = VBool Bool
    | VNum Int
    deriving (Show, Eq)

data Type
    = TBool
    | TNum
    deriving (Show, Eq)

data Bl = Bl {_isB :: [Instr], _rty :: [Type]} deriving (Show, Eq)

data Frame
    = BlockFrame {_bl :: Bl}
    | LoopFrame {_bl :: Bl, _allIs :: [Instr]}

$(makeLenses ''Bl)
$(makeLenses ''Frame)

typeOf :: Value -> Type
typeOf v = case v of
    VBool _ -> TBool
    VNum _  -> TNum
