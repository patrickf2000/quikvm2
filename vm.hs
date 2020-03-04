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

-- The main decoder
decoder op reader stack
    -- i_load
    | (chr 0x20) == op = do
        no <- readInt reader
        let s2 = no : stack
        return s2
        
    -- i_add
    | (chr 0x24) == op = do
        let no1 = head stack
        let stack2 = tail stack
        let no2 = head stack2
        let stack3 = tail stack2
        
        let answer = no1 + no2
        return (answer : stack3)
        
    -- i_print
    | (chr 0x29) == op = do
        let no = head stack
        putStrLn (show no)
        return stack
        
    -- i_pop
    | (chr 0x31) == op = do
        putStrLn "i_pop"
        return stack
        
    -- exit
    | (chr 0x10) == op = return stack
    
    -- lbl
    | (chr 0x11) == op = do
        no <- readInt reader
        putStrLn $ "lbl " ++ (show no)
        return stack
    
    -- jmp
    | (chr 0xA3) == op = do
        no <- readInt reader
        putStrLn $ "jmp " ++ (show no)
        return stack
        
    -- unknown instruction
    | otherwise = do
        putStrLn "idk"
        return stack

-- Load and execute the file
execute reader stack = do
    iseof <- hIsEOF reader
    if iseof
        then return()
        else do
            op <- hGetChar reader
            s2 <- decoder op reader stack
            execute reader s2

-- The main function
main = do
    reader <- openBinaryFile "first.bin" ReadMode
    execute reader []
    hClose reader
    
