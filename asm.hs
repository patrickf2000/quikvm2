-- The assembler
module Main(main) where

import System.IO
import Numeric
import Data.Char
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int

-- Types
data LblLoco = LblLoco {
      name :: String
    , loco :: Int
    } deriving (Show)
    
-- Write a single opcode to binary
write_opcode writer op = hPutChar writer (chr op)

-- Write an integer
writeInt writer i_str = do
    let i = read (i_str) :: Int32
    let i_bin = encode (i :: Int32)
    let i_bin2 = BL.toStrict i_bin
    BS.hPut writer i_bin2
    
-- Parse and write
parseLn tokens writer
    | (head tokens) == "i_load" = do
        write_opcode writer 0x20
        writeInt writer (last tokens)
    | (head tokens) == "i_add" = write_opcode writer 0x24
    | (head tokens) == "i_print" = write_opcode writer 0x29
    | (head tokens) == "exit" = write_opcode writer 0x10
    | otherwise = write_opcode writer 0x00
    
-- Pass 3 reading function
asmFile [] writer = hClose writer
asmFile (x:xs) writer = do
    parseLn (words x) writer
    asmFile xs writer
            
-- Pass 2
-- In this pass, we check labels and other label references
-- (mainly jumps)
findLabel [] _ = 0
findLabel (x:xs) lbl_name = do
    if (name x) == lbl_name
        then loco x
        else findLabel xs lbl_name

checkRef line labels
    | (head (words line)) == "lbl" = do
        let tokens = words line
        let no = findLabel labels (last tokens)
        "lbl " ++ (show no)
    | (head (words line)) == "jmp" = do
        let tokens = words line
        let no = findLabel labels (last tokens)
        "jmp " ++ (show no)
    | otherwise = line

loadRefs [] _ contents2 = reverse contents2
loadRefs (x:xs) labels contents2 = do
    let s = checkRef x labels
    loadRefs xs labels (s : contents2)
            
-- Pass 1
-- Check to see if we have a label
check_lbl :: [String] -> [LblLoco] -> Int -> [LblLoco]
check_lbl tokens loco ln_no
    | (head tokens) == "lbl" = do
        let name = (last tokens)
        let l = LblLoco {
            name = name
            ,loco = ln_no
        }
        l : loco
    | otherwise = loco
            
-- Pass 1 reading function
loadLabels :: [String] -> [LblLoco] -> Int -> [LblLoco]
loadLabels [] loco _ = reverse loco
loadLabels (x:xs) loco ln_no = do
    let loco2 = check_lbl (words x) loco ln_no
    loadLabels xs loco2 (ln_no + 1)
            
-- Load the file to memory
loadFile reader contents = do
    iseof <- hIsEOF reader
    if iseof
        then return (reverse contents)
        else do
            ln <- hGetLine reader
            loadFile reader (ln : contents)

-- The main function
main = do
    -- Load the file
    reader1 <- openFile "first.asm" ReadMode
    contents <- loadFile reader1 []
    hClose reader1
    
    -- Pass 1 (cache the labels)
    let lbl_loco = loadLabels contents [] 0
    
    -- Pass 2 (update references)
    let lbl_refs = loadRefs contents lbl_loco []

    -- Pass 3
    writer <- openBinaryFile "first.bin" WriteMode
    asmFile contents writer
    
