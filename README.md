Project: DKA-2-MKA
Student: Adam Rybansky
Login: xryban00

Description: I implemented the exact functionality that was required, nothing more, nothing less.
The code is divided into 4 modules: 
Main.hs - reads the command line arguments and executes proper actions
MyData.hs - here are declared all custom data types used in the algorithm
MyParser.hs - parses the starting DKA from input file or stdin and stores it in the DKA validate
MyConversion - the algorithm that converts DKA into his minimal version

The Main.hs and MyParser.hs modules are based on code for Turing Machine in school file system, with proper modifications.

I tried to cover all exceptions and corner cases that I thought of, but I may forgot some.

How to launch:

Step 1.
compile by writing "make"

Step 2.
run the program by writing "./flp21-fun -t /tests/test3input.txt"

or choose other input file from the 4 test files

or get input from the console by writing "./flp21-fun -t". In that instance you have to end the input with CTRL+D (which is the EOF character)

using argument "-i" instead of "-t" will only print the input DKA, without minimalizing it