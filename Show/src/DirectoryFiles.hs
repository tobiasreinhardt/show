-----------------------------------------------------------------------------
--
-- Module      :  DirectoryFiles
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

module DirectoryFiles (
    getFilesWith,
    getRecursivelyFilesWith,
    getFileExtension
) where

import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import Data.List (delete, elemIndices)

type DirectoryPath = String



getRecursivelyFilesWith :: (FilePath -> Bool) -> DirectoryPath -> IO [FilePath]
getRecursivelyFilesWith f x = do entries <- fmap (addPath x . removeDots) (getDirectoryContents x)
                                 files <- (ioFilter isFile entries)
                                 let condition = f . takeWhileNotFromEnd '/'
                                 directories <- ioFilter isDirectory entries
                                 if null directories
                                    then    return (filter condition files)
                                    else do rest <- mapM (getRecursivelyFilesWith f) directories
                                            return (filter condition files ++ concat rest)


getFilesWith :: (FilePath -> Bool) -> DirectoryPath -> IO [FilePath]
getFilesWith f x = do entries <- getDirectoryContents x
                      files <- fmap (addPath x) (ioFilter isFile entries)
                      let condition = f . takeWhileNotFromEnd '/'
                      return (filter condition files)


isFile :: String -> IO Bool
isFile = doesFileExist

isDirectory :: String -> IO Bool
isDirectory = doesDirectoryExist

takeWhileNotFromEnd :: Char -> String -> String
takeWhileNotFromEnd x xs = (reverse . takeWhile (x /=) . reverse) xs


removeDots :: [String] -> [String]
removeDots xs = delete ".." $ delete "." xs

addPath :: String -> [String] -> [String]
addPath x xs = map ((x ++ "/") ++) xs


ioFilter :: (a -> IO Bool) -> [a] -> IO [a]
ioFilter _ []     = return []
ioFilter f (x:xs) = do isTrue <- f x
                       rest <- ioFilter f xs
                       if isTrue then return (x:rest) else return (rest)

getFileExtension :: FilePath -> Maybe String
getFileExtension x = case elemIndexEnd '.' x of
                       Nothing -> Nothing
                       Just i  -> Just (drop i x)


elemIndexEnd :: Char -> String -> Maybe Int
elemIndexEnd x ys = case elemIndices x ys of
                      [] -> Nothing
                      us -> Just (last us)
