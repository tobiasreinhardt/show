# show
Show is a command line tool for quickly selecting a list of files from directory and run a different predefined command on the files. The same command is applied on every file with the same filename extension. The intension of the program is to help the user not to remember every command he/she used for one specific file type. He/she has to specify the command only once and the program remembers it. Additionally running the same command on multiple files is a lot faster.

For example, extracting multiple .zip files in a directory. First the user can specify the command “xarchiver –extract-to=.” And then type “show .zip”. As a result the program runs this command on every files ending with .zip in the current working directory.


#REQUIREMENTS
###Programs:
cabal-install, Haskell compiler

###Haskell modules:
	Not yet implemented	


#INSTALLATION
1. Go to ../show/Show/
2. run “cabal install”
3. Wherever cabal installed the executable. Add the destination to search path or copy the executable into a folder that is on the search path


