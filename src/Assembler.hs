-- The assembler
module Main(main) where

import System.IO
import System.Environment
import System.FilePath.Posix
import Numeric
import Data.Char
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as BL
import Data.Int

import Opcode
import Common

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
    
-- Writes a string
writeStr writer str = do
    let s_bin = BSU.fromString str
    BS.hPut writer s_bin
    
-- Remove quotes from a string
toString str = do
    let s1 = tail (str)
    let s2 = tail (reverse s1)
    reverse s2
    
-- Builds a list of string
buildStr [] s = reverse (s ++ "\"")
buildStr (x:xs) s = do
    let s2 = s ++ " " ++ x
    buildStr xs s2
    
-- Parse and write
parseLn tokens writer
    | (head tokens) == "i_load" = do
        write_opcode writer (toOpcode ILoad)
        writeInt writer (last tokens)
    | (head tokens) == "i_add" = write_opcode writer (toOpcode IAdd)
    | (head tokens) == "i_print" = write_opcode writer (toOpcode IPrint)
    | (head tokens) == "i_input" = write_opcode writer (toOpcode IInput)
    | (head tokens) == "i_pop" = write_opcode writer (toOpcode IPop)
    | (head tokens) == "s_load" = do
        write_opcode writer (toOpcode SLoad)
        let s1 = buildStr (tail tokens) []
        let str = toString (toString s1)
        let len = length str
        writeInt writer (show len)
        writeStr writer (reverse str)
    | (head tokens) == "s_print" = write_opcode writer (toOpcode SPrint)
    | (head tokens) == "exit" = write_opcode writer (toOpcode Exit)
    | (head tokens) == "lbl" = do
        write_opcode writer (toOpcode Lbl)
        writeInt writer (last tokens)
    | (head tokens) == "jmp" = do
        write_opcode writer (toOpcode Jmp)
        writeInt writer (last tokens)
    | otherwise = write_opcode writer (toOpcode None)
    
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
            let len = length ln
            if len == 0
                then loadFile reader contents
                else loadFile reader (ln : contents)
        
-- The main function
main = do
    args <- getArgs
    checkArgs args
    let path = args !! 0
    let outName = (takeBaseName path) ++ ".bin"
    
    -- Load the file
    reader1 <- openFile path ReadMode
    contents <- loadFile reader1 []
    hClose reader1
    
    -- Pass 1 (cache the labels)
    let lbl_loco = loadLabels contents [] 0
    
    -- Pass 2 (update references)
    let contents2 = loadRefs contents lbl_loco []

    -- Pass 3
    writer <- openBinaryFile outName WriteMode
    asmFile contents2 writer
    
