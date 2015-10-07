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

-- TODO Add a module describtion in every file
-- WHAT
-- WHY
-- HOW
-- MAIN IMPLEMENTATION IDEAS

-- TODO Add accurate module dependecy versions in all cabal files
-- TODO Add color to output messages

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
l = Both 'l' "list"

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
\  -l, --list        lists the current commands specified\n\
\                    for the file-extensions\n\
\  -h, --help        print this help\n\
\\n\
\By default a file is selected if even one of the given\n\
\patterns is a substring of the filename.\n\n"

definedOptions :: [(Option, NeedForArgument)]
definedOptions = [(r, None),
                  (d, Compulsory),
                  (p, Compulsory),
                  (g, None),
                  (h, None),
                  (l, None)]

-- should commandToFileFormat be saved after each successfull modification ?
-- commandToFileformat should newer fall into a state where it is unusable

type Extension = String


main :: IO ()
main = do marshalled <- marshalArguments definedOptions
          let showHelp            = isOptionFlagged h marshalled
          let listCommands        = isOptionFlagged l marshalled
          let workingDirectory    = fetchArgumentsOfOption p marshalled
          let isRecursive         = isOptionFlagged r marshalled
          let patterns            = fetchNonOptionArguments marshalled
          let isRegex             = isOptionFlagged g marshalled
          let fileFilterCondition = getFileFilterCondition isRegex patterns
          let extensionsBeRemoved = fetchArgumentsOfOption d marshalled
          when showHelp $ do putStrLn helpMessage; exitSuccess
          cfgFilepath <- getCfgFilepath
          createCfgFileIfMissing cfgFilepath
          extToComm  <- fmap getDefaultSection (readConfiguration cfgFilepath)
          when listCommands $ do printExtToComm extToComm; exitSuccess
          extToComm2 <- removeExtensions extToComm extensionsBeRemoved
          exitIf (null patterns) (putStrLn "No patterns given, exiting...")
          setWorkingDirectory workingDirectory
          files      <- getFilepaths isRecursive fileFilterCondition
          exitIf (null files) (putStrLn "No file matched the given patterns, exiting...")
          sysCalls   <- associateCommands files extToComm2 cfgFilepath
          _ <- mapM system sysCalls
          return ()





createCfgFileIfMissing :: String -> IO()
createCfgFileIfMissing x =
  do fileExist <- doesFileExist x
     unless fileExist $ createCfgFile x


printExtToComm :: [Property] -> IO ()
printExtToComm []     = putStrLn ""
printExtToComm ((x1,x2):xs) =
  if null x2
  then do putStrLn (x1 ++ " -> IGNORE"); printExtToComm xs
  else do putStrLn (x1 ++ " -> " ++ x2 ++ " [filename]"); printExtToComm xs


createCfgFile :: String -> IO()
createCfgFile x =
  do putStrLn $ colorify yellow "Configuration file does not exist\n" ++
               "Attempting to automatically create a new one..."
     h <- exitIfFails (openFile x WriteMode) "Failed to create file"
     hClose h
     putStrLn $ colorify green ("A new configuration file was created in\n" ++ x)
     return ()

-- TODO save the configuration after removing
removeExtensions :: [Property] -> [Extension] -> IO [Property]
removeExtensions xs [] = return xs
removeExtensions xs (y:ys) =
  do let extToComm = removeProperty y xs
     if length extToComm == length xs
       then putStrLn $ colorify yellow ("Can't remove command for extension '" ++ y ++ "'. It does not exist")
       else putStrLn $ colorify green ("Removed command for extension '" ++ y ++ "'")
     removeExtensions extToComm ys


-- TODO add fatal error message if write to configuration file goes wrong
associateCommands :: [FilePath] -> [Property] -> String -> IO [String]
associateCommands [] _ z      = return []
associateCommands (x:xs) ys z =
  case getFileExtension x of
    Nothing -> associateCommands xs ys z
    Just u -> case lookup u ys of
                Just v  -> do if null v
                                then associateCommands xs ys z
                                else do otherCommands <- associateCommands xs ys z
                                        return ((v ++ " " ++ x ++ " &") : otherCommands)
                Nothing -> do newCommand <- createCommand u
                              let extToComm = (u, newCommand) : ys
                              _ <- writeConfiguration z [("",extToComm)]
                              if null newCommand
                                then associateCommands xs extToComm z
                                else do otherCommands <- associateCommands xs extToComm z
                                        return ((newCommand ++ " " ++ x ++ " &") : otherCommands)

createCommand :: String -> IO String
createCommand xs = do putStrLn ("new extension '" ++ xs ++ "'")
                      putStrLn "type: 'i' to ignore, 'c' to set command, press enter without anything to skip"
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
getCfgFilepath = do home <- exitIfFails (getEnv "HOME") "Could not get environment variable HOME"
                    return (home ++ "/.config/show.conf")


getFilepaths ::  Bool -> (String -> Bool) -> IO [String]
getFilepaths y f = if y
                   then getRecursivelyFilesWith f "."
                   else getFilesWith f "."


exitIf :: Bool -> IO a -> IO ()
exitIf b x = when b $ do _ <- x; exitSuccess


exitIfFails :: IO a -> String -> IO a
exitIfFails f m = catch f ((\e -> do putStrLn $ colorify red ("An exception was caught: " ++ show e)
                                     putStrLn $ colorify red ("FATAL ERROR: " ++ m)
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

green :: String
green = "\x1b[32m"

uncolor :: String
uncolor = "\x1b[0m"

red :: String
red = "\x1b[31m"

yellow :: String
yellow = "\x1b[33m"

colorify :: String -> String -> String
colorify x y = x ++ y ++ uncolor
