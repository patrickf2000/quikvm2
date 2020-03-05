module Opcode(OpCode(..), toOpcode, toChar) where

import Data.Char

-- Instructions
data OpCode = ILoad | IAdd | IPrint | IPop
        | Exit | Lbl | Jmp | None
        deriving(Show, Eq)
        
toOpcode op
    | op == ILoad = 0x20
    | op == IAdd = 0x24
    | op == IPrint = 0x29
    | op == IPop = 0x31
    | op == Exit = 0x10
    | op == Lbl = 0x11
    | op == Jmp = 0xA3
    | otherwise = 0x00
    
toChar op
    | op == ILoad = (chr 0x20)
    | op == IAdd = (chr 0x24)
    | op == IPrint = (chr 0x29)
    | op == IPop = (chr 0x31)
    | op == Exit = (chr 0x10)
    | op == Lbl = (chr 0x11)
    | op == Jmp = (chr 0xA3)
    | otherwise = (chr 0x00)
