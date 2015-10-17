# show
Show is a command line tool for quickly selecting a list of files from directory and run a different predefined command on the files. The Intension of the program is to help the user not to remember every command he/she used for one specific file type as he/she has to specify the command only once and the program remembers it. Additionally running the same command on multiple files is a lot faster.

For example, extracting multiple .zip files in a directory: First the user can specify the command “xarchiver –extract-to=.” to be executed on .zip files. And then type “show .zip” to the terminal. As a result the program runs this command on every files ending with .zip in the current working directory.


##INSTALLATION FROM SOURCE

###REQUIREMENTS
#####Programs:
cabal-install, Haskell compiler

#####Haskell modules:
base -any, deepseq >= 1.2.0.0, hspec -any, split -any, regex-compat -any, directory -any, process -any

###BUILD
1. Go to ../show/Show/
2. run “cabal install”
3. Wherever cabal installed the executable add the destination to search path or copy the executable into a folder that is on the search path
4. run "show --help" for more information about features
