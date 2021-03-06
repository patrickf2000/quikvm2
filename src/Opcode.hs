module Opcode(OpCode(..), toOpcode, toChar) where

import Data.Char

-- Instructions
data OpCode = ILoad | IStore | IVar | ILoadVar
        | IAdd | ISub | IMul | IDiv | IMod
        | IPrint | IInput | IPop
        | SLoad | SPrint | SPop
        | Exit | Lbl | Jmp | None
        deriving(Show, Eq)
        
toOpcode op
    | op == ILoad = 0x20
    | op == IStore = 0x21
    | op == IVar = 0x22
    | op == ILoadVar = 0x23
    | op == IAdd = 0x24
    | op == ISub = 0x25
    | op == IMul = 0x26
    | op == IDiv = 0x27
    | op == IMod = 0x28
    | op == IPrint = 0x29
    | op == IInput = 0x30
    | op == IPop = 0x31
    | op == SLoad = 0x60
    | op == SPrint = 0x61
    | op == SPop = 0x62
    | op == Exit = 0x10
    | op == Lbl = 0x11
    | op == Jmp = 0xA3
    | otherwise = 0x00
    
toChar op = chr (toOpcode op)
