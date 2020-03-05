module Opcode(OpCode(..), toOpcode, toChar) where

import Data.Char

-- Instructions
data OpCode = ILoad | IAdd | IPrint | IInput | IPop
        | SLoad | SPrint
        | Exit | Lbl | Jmp | None
        deriving(Show, Eq)
        
toOpcode op
    | op == ILoad = 0x20
    | op == IAdd = 0x24
    | op == IPrint = 0x29
    | op == IInput = 0x30
    | op == IPop = 0x31
    | op == SLoad = 0x60
    | op == SPrint = 0x61
    | op == Exit = 0x10
    | op == Lbl = 0x11
    | op == Jmp = 0xA3
    | otherwise = 0x00
    
toChar op = chr (toOpcode op)
