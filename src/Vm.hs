-- The virtual machine
module Main(main) where

import System.IO
import System.Environment
import Data.Char
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Binary.Get as B
import Data.Int
import Data.Binary as B

import Opcode
import Common

-- Represents an instruction
data Instr = Instr {
      opcode    :: Char
    , iArg      :: Int32
    , sArg      :: String
    } deriving (Show)
    
-- Decodes an individual instruction
decoder instr stack pc
    -- i_load
    | (toChar ILoad) == (opcode instr) = do
        let i = iArg instr
        let s2 = (show i) : stack
        return ((show pc2) : s2)
    
    -- i_add
    | (toChar IAdd) == (opcode instr) = do
        let n1 = read (head stack) :: Int
        let stack2 = tail stack
        let n2 = read (head stack2) :: Int
        let stack3 = tail stack2
        let answer = n1 + n2
        let stack_f = (show answer) : stack3
        return ((show pc2) : stack_f)
    
    -- i_print
    | (toChar IPrint) == (opcode instr) = do
        putStrLn (head stack)
        return ((show pc2) : stack)
    
    -- i_pop
    | (toChar IPop) == (opcode instr) = do
        putStrLn "i_pop"
        return ((show pc2) : stack)
        
    -- s_load
    | (toChar SLoad) == (opcode instr) = do
        let s = sArg instr
        let s2 = s : stack
        return ((show pc2) : s2)
    
    -- s_print
    | (toChar SPrint) == (opcode instr) = do
        putStrLn (head stack)
        return ((show pc2) : stack)
    
    -- exit
    | (toChar Exit) == (opcode instr) = return stack
    
    -- lbl
    | (toChar Lbl) == (opcode instr) = return ((show pc2) : stack)
    
    -- jmp
    | (toChar Jmp) == (opcode instr) = do
        let loco = iArg instr
        return ((show loco) : stack)
    
    | otherwise = return stack
    where
        pc2 = pc + 1
    
-- The main execution function
execute contents stack pc = do
    if pc == (length contents)
        then return ()
        else do
            let instr = contents !! pc
            stack2 <- decoder instr stack pc
            let inc = read (head stack2) :: Int
            execute contents (tail stack2) (inc)

-- Reads an integer from the file
readInt reader = do
    b1 <- hGetChar reader
    b2 <- hGetChar reader
    b3 <- hGetChar reader
    b4 <- hGetChar reader
    let no_str = BSU.fromString [b1, b2, b3, b4] :: BS.ByteString
    let no_str_l = BL.fromStrict no_str
    let no = B.decode no_str_l :: Int32
    return ()
    return no
    
-- Reads a string from a file
buildStr str reader 0 = return (reverse str)
buildStr str reader i = do
    c <- hGetChar reader
    buildStr (c : str) reader (i - 1)
    
readStr reader len = do
    s <- buildStr [] reader len
    return s
    
-- Returns whether we have an argument instruction
isIntArg op
    -- i_load
    | (toChar ILoad) == op = True
    -- jmp
    | (toChar Jmp) == op = True
    -- lbl
    | (toChar Lbl) == op = True
    -- All others
    | otherwise = False
    
-- Builds an instruction
buildInstr op reader contents
    -- Integer argument instructions
    | (isIntArg op) == True = do
        no <- readInt reader
        let instr = Instr {
            opcode = op,
            iArg = no,
            sArg = ""
        }
        let c2 = instr : contents
        return c2
        
    -- s_load
    | (toChar SLoad) == op = do
        no <- readInt reader
        str <- readStr reader no
        let instr = Instr {
            opcode = op,
            iArg = no,
            sArg = str
        }
        let c2 = instr : contents
        return c2
        
    -- exit
    | (toChar Exit) == op = return contents
    
    -- null
    | (toChar None) == op = return contents
        
    -- Other instruction instruction
    | otherwise = do
        let instr = Instr {
            opcode = op,
            iArg = 0,
            sArg = ""
        }
        let c2 = instr : contents
        return c2
    
-- Load and execute the file
loadFile reader contents = do
    iseof <- hIsEOF reader
    if iseof
        then return (reverse contents)
        else do
            op <- hGetChar reader
            s2 <- buildInstr op reader contents
            loadFile reader s2

-- The main function
main = do
    args <- getArgs
    checkArgs args
    let path = args !! 0

    reader <- openBinaryFile path ReadMode
    contents <- loadFile reader []
    execute contents [] 0
    hClose reader
    
