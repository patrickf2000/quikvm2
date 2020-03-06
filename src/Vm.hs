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
import qualified Data.Sequence as List
import Data.Foldable (toList)

import Opcode
import Common

-- Represents an instruction
data Instr = Instr {
      opcode    :: Char
    , iArg      :: Int32
    , sArg      :: String
    } deriving (Show)
    
-- Represents runtime data
data RetData = RetData {
      rstack     :: [String]
    , rvars      :: [String]
    , rpc        :: Int
    } deriving (Show)
    
-- Builds runtime data
buildRData stack vars pc = RetData {rstack=stack, rvars=vars, rpc=pc}
    
-- Returns whether we have a math function
isMath instr
    | (toChar IAdd) == (opcode instr) = True
    | (toChar ISub) == (opcode instr) = True
    | (toChar IMul) == (opcode instr) = True
    | (toChar IDiv) == (opcode instr) = True
    | (toChar IMod) == (opcode instr) = True
    | otherwise = False
    
-- Solves a math function
decodeMath instr rdata
    | (toChar IAdd) == (opcode instr) = do
        let answer = n1 + n2
        let stack_f = (show answer) : stack3
        return (buildRData stack_f vars (pc+1))
    | (toChar ISub) == (opcode instr) = do
        let answer = n1 - n2
        let stack_f = (show answer) : stack3
        return (buildRData stack_f vars (pc+1))
    | (toChar IMul) == (opcode instr) = do
        let answer = n1 * n2
        let stack_f = (show answer) : stack3
        return (buildRData stack_f vars (pc+1))
    | (toChar IDiv) == (opcode instr) = do
        let answer = n1 `div` n2
        let stack_f = (show answer) : stack3
        return (buildRData stack_f vars (pc+1))
    | (toChar IMod) == (opcode instr) = do
        let answer = n1 `rem` n2
        let stack_f = (show answer) : stack3
        return (buildRData stack_f vars (pc+1))
    where
        stack = rstack rdata
        vars = rvars rdata
        pc = rpc rdata
        n1 = read (head stack) :: Int
        stack2 = tail stack
        n2 = read (head stack2) :: Int
        stack3 = tail stack2
        
-- Decodes an individual instruction
decoder instr rdata
    -- i_load
    | (toChar ILoad) == (opcode instr) = do
        let i = iArg instr
        let s2 = (show i) : stack
        return (buildRData s2 vars (pc+1))
        
    -- i_var
    | (toChar IVar) == (opcode instr) = do
        let i = iArg instr
        let v2 = (show i) : vars
        return (buildRData stack v2 (pc+1))
    
    -- i_store
    | (toChar IStore) == (opcode instr) = do
        let i = fromEnum (iArg instr)
        let no = head stack
        let s2 = tail stack
        let updateVars = List.update i no $ List.fromList vars
        let vars2 = toList updateVars :: [String]
        return (buildRData s2 vars2 (pc+1))
    
    -- i_load_var
    | (toChar ILoadVar) == (opcode instr) = do
        let i = fromEnum (iArg instr)
        let no = vars !! i
        let s2 = no : stack
        return (buildRData s2 vars (pc+1))
    
    -- i_math
    | (isMath instr) == True = (decodeMath instr rdata)
    
    -- i_print
    | (toChar IPrint) == (opcode instr) = do
        putStrLn (head stack)
        return (buildRData stack vars (pc+1))
        
    -- i_input
    | (toChar IInput) == (opcode instr) = do
        str <- getLine
        let i = read str :: Int
        let s2 = (show i) : stack
        return (buildRData s2 vars (pc+1))
    
    -- i_pop
    | (toChar IPop) == (opcode instr) = do
        putStrLn "i_pop"
        return (buildRData stack vars (pc+1))
        
    -- s_load
    | (toChar SLoad) == (opcode instr) = do
        let s = sArg instr
        let s2 = s : stack
        return (buildRData s2 vars (pc+1))
    
    -- s_print
    | (toChar SPrint) == (opcode instr) = do
        putStrLn (head stack)
        return (buildRData stack vars (pc+1))
        
    -- s_pop
    | (toChar SPop) == (opcode instr) = do
        let s2 = tail stack
        return (buildRData s2 vars (pc+1))
    
    -- exit
    | (toChar Exit) == (opcode instr) = return (buildRData stack vars pc)
    
    -- lbl
    | (toChar Lbl) == (opcode instr) = return (buildRData stack vars (pc+1))
    
    -- jmp
    | (toChar Jmp) == (opcode instr) = do
        let loco = fromEnum (iArg instr)
        return (buildRData stack vars loco)
    
    | otherwise = return (buildRData stack vars pc)
    where
        stack = rstack rdata
        vars = rvars rdata
        pc = rpc rdata
    
-- The main execution function
execute contents rdata = do
    let pc = rpc rdata
    if pc == (length contents)
        then return ()
        else do
            let instr = contents !! pc
            rdata2 <- decoder instr rdata
            execute contents rdata2

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
    -- i_var
    | (toChar IVar) == op = True
    -- i_store
    | (toChar IStore) == op = True
    -- i_load_var
    | (toChar ILoadVar) == op = True
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
    
    let rdata = RetData { rstack=[], rvars=[], rpc=0}

    reader <- openBinaryFile path ReadMode
    contents <- loadFile reader []
    execute contents rdata
    hClose reader
    
