-- The virtual machine
module Main(main) where

import System.IO
import Data.Char
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Binary.Get as B
import Data.Int
import Data.Binary as B

-- Represents an instruction
data Instr = Instr {
      opcode    :: Char
    , iArg      :: Int32
    } deriving (Show)
    
-- Decodes an individual instruction
decoder instr stack
    -- i_load
    | (chr 0x20) == (opcode instr) = do
        let i = iArg instr
        let s2 = (show i) : stack
        return s2
    
    -- i_add
    | (chr 0x24) == (opcode instr) = do
        let n1 = read (head stack) :: Int
        let stack2 = tail stack
        let n2 = read (head stack2) :: Int
        let stack3 = tail stack2
        let answer = n1 + n2
        let stack_f = (show answer) : stack3
        return stack_f
    
    -- i_print
    | (chr 0x29) == (opcode instr) = do
        putStrLn (head stack)
        return stack
    
    -- i_pop
    | (chr 0x31) == (opcode instr) = do
        putStrLn "i_pop"
        return stack
    
    -- exit
    | (chr 0x10) == (opcode instr) = return stack
    
    -- lbl
    | (chr 0x11) == (opcode instr) = do
        putStrLn "lbl"
        return stack
    
    -- jmp
    | (chr 0xA3) == (opcode instr) = do
        putStrLn "jmp"
        return stack
    
    | otherwise = return stack
    
-- The main execution function
execute contents stack pc = do
    if pc == (length contents)
        then return ()
        else do
            let instr = contents !! pc
            stack2 <- decoder instr stack
            execute contents stack2 (pc + 1)

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
    
-- Builds an instruction
buildInstr op reader contents
    -- i_load
    | (chr 0x20) == op = do
        no <- readInt reader
        let instr = Instr {
            opcode = op,
            iArg = no
        }
        let c2 = instr : contents
        return c2
        
    -- i_add
    | (chr 0x24) == op = do
        let instr = Instr {
            opcode = op,
            iArg = 0
        }
        let c2 = instr : contents
        return c2
        
    -- i_print
    | (chr 0x29) == op = do
        let instr = Instr {
            opcode = op,
            iArg = 0
        }
        let c2 = instr : contents
        return c2
        
    -- i_pop
    | (chr 0x31) == op = do
        let instr = Instr {
            opcode = op,
            iArg = 0
        }
        let c2 = instr : contents
        return c2
        
    -- exit
    | (chr 0x10) == op = return contents
    
    -- lbl
    | (chr 0x11) == op = do
        no <- readInt reader
        let instr = Instr {
            opcode = op,
            iArg = no
        }
        let c2 = instr : contents
        return c2
    
    -- jmp
    | (chr 0xA3) == op = do
        no <- readInt reader
        let instr = Instr {
            opcode = op,
            iArg = no
        }
        let c2 = instr : contents
        return c2
        
    -- unknown instruction
    | otherwise = return contents
    
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
    reader <- openBinaryFile "first.bin" ReadMode
    contents <- loadFile reader []
    execute contents [] 0
    hClose reader
    
