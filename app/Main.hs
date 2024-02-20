module Main where

{--    

       All I can say is Post orgasmic illness syndrome and depression suck. Other than that, 
       This program is in the works but can currently work only I believe for all valid haskell Chars but, 
       I don't think it works for many unicode code points. When I stop having POIS symptoms maybe I will get my cognition back 
       and be able to accomplish something with my life and this program again. Just have to wait 7 days! Oh boy how fun! 
       - Alec 
--}

import Data.Char (ord, chr)
import Numeric (showHex, readHex) 
import Control.Monad (forever) 
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout, hPutStr, stderr)
import Control.Exception (catch, IOException) 
import Data.Maybe (fromJust)

 -- Function to convert a character to its hexadecimal Unicode code point --}



charToUni :: Char -> String
charToUni c = "U+" ++ (appendZeros $ showHex (ord c) "") where
    appendZeros s 
      | length s > 4 = s
      | otherwise = replicate (4 - length s) '0' ++ s



--------------------------------------------

 -- Functions to convert a unicode string back to a char. 

uniToChar :: String -> Maybe Char 
uniToChar s = 
  maybeChar (takeHexNumber $ readHex (filter (`elem` "0123456789abcdef") s) )

takeHexNumber :: [(Int, String)] -> Maybe Int
takeHexNumber arg = do
      case arg of
        [(a , _)] -> Just a
        _         -> Nothing 

maybeChar :: Maybe Int -> Maybe Char
maybeChar (Just x) = Just (chr x) 
maybeChar _ = Nothing

-- Main program 

main :: IO ()
main = do
  putStrLn "Welcome to Char to UniCode program!" 
  putStrLn $ "1) Interactive program that will read from a text file, and create a list of unicode code points which you can print or write to a file." 
  putStrLn $ "2) Interactive program that will convert your userinput to unicode code points from command line. "
  putStrLn $ "3) Interactive program that will read from a text file with unicode, and convert unicode back to regular characters." 
  forever $ do putStr "Enter 1 2 3 (type 'stop' or 'exit' to end the program): "
               hFlush stdout 
               input <- getLine
               case input of
                "1" -> fileLoop 
                "2" -> userinputLoop 
                "3" -> unifileLoop 
                "stop" -> exitSuccess
                "exit" -> exitSuccess
                _  -> putStrLn "Error invalid input." 


fileLoop :: IO () 
fileLoop = forever $ do
  putStrLn "Enter a file name (type 'stop' or 'exit' to exit or 'main' for main menu.): "
  filename <- getLine 
  case filename  of
   "stop" -> putStrLn "Exiting the program."  >> exitSuccess
   "exit" -> putStrLn "Exiting the program."  >> exitSuccess
   "main" -> main
   _      -> do
              contents <- catch (readFile filename) (\e -> do hPutStr stderr $ "Error could not open file: " ++ show (e :: IOException)
                                                              return "" ) 
              let unicodeContents = foldr (\c acc -> (charToUni c) ++ " " ++ acc) "" contents
              putStrLn "1) Print unicode translation to the screen."
              putStrLn "2) Create a new file with unicode translation." 
              input <- getLine
              case input of
               "1" -> putStrLn unicodeContents
               "2" -> do
                       putStrLn "Enter file name: " 
                       newFile  <- getLine 
                       writeFile newFile  unicodeContents
               _   -> putStrLn "Error: Enter either 1 or 2." 

-- Function to process user input 

userinputLoop :: IO () 
userinputLoop = forever $ do
    putStrLn "Enter a character (type 'stop' or 'exit' to exit or 'main' for main menu.): "
    input <- getLine
    case input of
     "stop" -> putStrLn "Exiting the program." >> exitSuccess
     "exit" -> putStrLn "Exiting the program." >> exitSuccess
     "main" -> main
     [c]    -> putStrLn $ "The unicode point of '" ++ [c] ++ "' is " ++ charToUni c
     _      -> putStrLn "Error: Input should be a single character." 

-- Function to work with unicode files.
unifileLoop :: IO () 
unifileLoop = forever $ do
    putStrLn "Enter a file name (type 'stop' or 'exit' to exit or 'main' for main menu.): "
    fileName <- getLine 
    case fileName  of
     "stop" -> putStrLn "Exiting the program."  >> exitSuccess
     "exit" -> putStrLn "Exiting the program."  >> exitSuccess
     "main" -> main
     _      -> do
            contents <- catch (readFile fileName) $ \e -> do hPutStr stderr $ "Error: " ++ show (e :: IOException)
                                                             return ""
                                                        
            let uniWords = words contents
            let maybeChrContents = (foldr (\s acc -> (uniToChar s) :  acc) []  uniWords)
            putStrLn "1) Print chr translation to the screen."
            putStrLn "2) Create a new file with chr translation." 
            input <- getLine
            case input of
             "1" ->    if (any (== Nothing) maybeChrContents) 
                       then putStrLn "The file you are reading is not entirely in unicode code points." 
                       else (putStrLn $ map fromJust maybeChrContents)

             "2" ->     if (any (== Nothing) maybeChrContents) 
                        then putStrLn "The file you are reading is not entirely in unicode code points." 
                        else do putStrLn "Enter file name: " 
                                filename <- getLine 
                                writeFile filename (map fromJust maybeChrContents)
             _   -> putStrLn "Error: Enter either 1 or 2." 
