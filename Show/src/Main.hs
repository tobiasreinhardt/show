-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

-- TODO I don't remember why these two lines are here, but everything seems to work without them
--{-# LANGUAGE CPP             #-}
--{-# LANGUAGE TemplateHaskell #-}

-- TODO Add a module describtion in every file
-- WHAT
-- WHY
-- HOW
-- MAIN IMPLEMENTATION IDEAS

-- TODO Add accurate module dependecy versions in all cabal files

module Main (
    main
) where

import           CLIArguments
import           Control.Monad
import           Data.List
import           DirectoryFiles
import           IniConfiguration
import           System.Directory (setCurrentDirectory, doesFileExist)
import           System.Exit
import           System.Process
import           System.Environment
import           Text.Regex
import           Data.Maybe
import           System.IO (openFile, hClose, Handle, IOMode (WriteMode))
import           Control.Exception

r = Both 'r' "recursive"
d = Both 'd' "delete"
p = Both 'p' "path"
g = Both 'g' "regex"
h = Both 'h' "help"

helpMessage :: String
helpMessage="\
\Usage: show [OPTION]... [PATTERN]...\n\
\Open files with user predefined programs.\n\n\
\  -d, --delete      delete a defined command for a file\n\
\                    extension\n\
\  -r, --recursive   search dirctory recursively for files\n\
\  -p, --path        specifies the directory wherefrom the\n\
\                    files are opened\n\
\  -g, --regex       files are selected if their names matches\n\
\                    user given POSIX regular expressions\n\
\  -h, --help        print this help\n\
\\n\
\By default a file is selected if even one of the given\n\
\patterns is a substring of the filename.\n\n"

definedOptions :: [(Option, NeedForArgument)]
definedOptions = [(r, None),
                  (d, Compulsory),
                  (p, Compulsory),
                  (g, None),
                  (h, None)]

-- should commandToFileFormat be saved after each successfull modification ?
-- commandToFileformat should newer fall into a state where it is unusable

type Extension = String

--TODO Check if the program throws an error whenewer an argument is not defined

main :: IO ()
main = do marshalled <- marshalArguments definedOptions
          let showHelp            = isOptionFlagged h marshalled
          when showHelp $ do putStrLn helpMessage
                             exitSuccess
          let workingDirectory    = fetchArgumentsOfOption p marshalled
          let isRecursive         = isOptionFlagged r marshalled
          let patterns            = fetchNonOptionArguments marshalled
          let isRegex             = isOptionFlagged g marshalled
          let fileFilterCondition = getFileFilterCondition isRegex patterns
          let extensionsBeRemoved = fetchArgumentsOfOption d marshalled
          cfgFilepath <- getCfgFilepath
          createCfgFileIfMissing cfgFilepath
          extToComm  <- fmap getDefaultSection (readConfiguration cfgFilepath)
          extToComm2 <- removeExtensions extToComm extensionsBeRemoved
          exitIf (null patterns) (putStrLn "no patterns given, exiting...")
          setWorkingDirectory workingDirectory
          files      <- getFilepaths isRecursive fileFilterCondition
          exitIf (null files) (putStrLn "no file matched the given patterns, exiting...")
          sysCalls   <- associateCommands files extToComm2
          _ <- mapM system sysCalls
          return ()

createCfgFileIfMissing :: String -> IO()
createCfgFileIfMissing x =
  do fileExist <- doesFileExist x
     unless fileExist $ createCfgFile x

createCfgFile :: String -> IO()
createCfgFile x =
  do putStrLn "Configuration file does not exist\n\
              \Attempting to automatically create a new one"
     h <- exitIfFailed (openFile x WriteMode) "Failed to create file"
     hClose h
     putStrLn ("A new configuration file was created in\n" ++ x)
     return ()

removeExtensions :: [Property] -> [Extension] -> IO [Property]
removeExtensions xs [] = return xs
removeExtensions xs (y:ys) =
  do let extToComm = removeProperty y xs
     if length extToComm == length xs
       then do putStrLn ("WARNING: Can't remove command for extension '" ++ y ++ "'")
               putStrLn "          It does not exist"
       else putStrLn ("Removed command for extension '" ++ y ++ "'")
     removeExtensions extToComm ys


-- TODO add configuration file as argument
-- TODO add fatal error message if write to configuration file goes wrong
associateCommands :: [FilePath] -> [Property] -> IO [String]
associateCommands [] _      = return []
associateCommands (x:xs) ys =
  case getFileExtension x of
    Nothing -> associateCommands xs ys
    Just u -> case lookup u ys of
                Just v  -> do if null v
                                then associateCommands xs ys
                                else do otherCommands <- associateCommands xs ys
                                        return ((v ++ " " ++ x ++ " &") : otherCommands)
                Nothing -> do newCommand <- createCommand u
                              let extToComm = (u, newCommand) : ys
                              _ <- writeConfiguration "/home/tobias/show.conf" [("",extToComm)]
                              if null newCommand
                                then associateCommands xs extToComm
                                else do otherCommands <- associateCommands xs extToComm
                                        return ((x ++ " " ++ newCommand ++ " &") : otherCommands)



createCommand :: String -> IO String
createCommand xs = do putStrLn ("new extension '" ++ xs ++ "'")
                      putStrLn "type: 'i' for ignore, 'c' for command"
                      queryCommand

queryCommand :: IO String
queryCommand = do input <- getLine
                  run [(input == "i", return ""),
                       (input == "c", do putStrLn "command to be executed:"; getLine),
                       (True, queryCommand)]


run :: [(Bool, IO a)] -> IO a
run []            = error ""
run ((True, x):_) = x
run (_:xs)        = run xs

getCfgFilepath :: IO String
getCfgFilepath = do home <- exitIfFailed (getEnv "HOME") "Could not get environment variable HOME"
                    return (home ++ "/.config/show.conf")

getFilepaths ::  Bool -> (String -> Bool) -> IO [String]
getFilepaths y f = if y
                   then getRecursivelyFilesWith f "."
                   else getFilesWith f "."


exitIf :: Bool -> IO a -> IO ()
exitIf b x = when b $ do _ <- x; exitSuccess

exitIfFailed :: IO a -> String -> IO a
exitIfFailed f m = catch f ((\e -> do putStrLn ("An exception was caught: " ++ show e)
                                      putStrLn ("FATAL ERROR: " ++ m)
                                      exitFailure) :: SomeException -> IO a)

getFileFilterCondition :: Bool -> [String] -> (String -> Bool)
getFileFilterCondition True ys = containsPattern (map mkRegex ys)
getFileFilterCondition False ys = containsSubString ys


setWorkingDirectory :: [String] -> IO ()
setWorkingDirectory []     = return ()
setWorkingDirectory (x:_) = setCurrentDirectory x


containsPattern :: [Regex] -> String -> Bool
containsPattern [] _     = False
containsPattern (x:xs) y = case matchRegex x y of
                             Just _ -> True
                             Nothing -> containsPattern xs y

containsSubString :: [String] -> String -> Bool
containsSubString [] _     = False
containsSubString (x:xs) y = x `isInfixOf` y || containsSubString xs y
