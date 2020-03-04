-- The assembler
module Main(main) where

import System.IO
import Numeric
import Data.Char
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int
    
-- Write a single opcode to binary
write_opcode writer op = hPutChar writer (chr op)
    
-- Parse and write
parse_ln tokens writer
    | (head tokens) == "i_load" = do
        write_opcode writer 0x20
        
        let i = read (last tokens) :: Int32
        let i_bin = encode (i :: Int32)
        let i_bin2 = BL.toStrict i_bin
        BS.hPut writer i_bin2
    | (head tokens) == "i_add" = write_opcode writer 0x24
    | (head tokens) == "i_print" = write_opcode writer 0x29
    | (head tokens) == "exit" = write_opcode writer 0x10
    | otherwise = write_opcode writer 0x00
    
-- Main file reading function
read_file2 reader writer = do
    iseof <- hIsEOF reader
    if iseof
        then return ()
        else do
            ln <- hGetLine reader
            parse_ln (words ln) writer
            read_file2 reader writer

-- The main function
main = do
    writer <- openBinaryFile "first.bin" WriteMode

    reader <- openFile "first.asm" ReadMode
    read_file2 reader writer
    
    hClose reader
    hClose writer
