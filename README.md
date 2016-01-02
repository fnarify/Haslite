# Haslite
Simple compiler for made-up language Haslite which is an expression-based language.

**How to run**

Windows: Use sbt.bat.

Mac/Unix: Use sbt.

For both clone the repository and then open a command prompt in the directory and run the command 'sbt'.
Once all required files have been acquired type run and enter your commands into the REPL.

For unit testing you only need to use the command test to run all tests located in the src folder, if you want to run a designated file of tests you can use the command **test-only *FILENAME**. Otherwise if you want to run the other small tests in the folder 'test' run the command **run test/TESTNAME.hsl**.

This compiler processes simple constructs such as "3 + 4" and returns the value of said expression. There is support for addition, multiplication, subtraction and division. The only types available are integers, booleans and closures.

**Constructs**
```
IDN     - any combination of lower or upper case characters a-z and digits 0-9 as long as the identifier is not a keyword.
BOOL    - TRUE or FALSE
INT     - any integer value.
DEFN    - IDN = EXP e.g. a = 3 creates a map with name 'a' having value 3.
IF EXP  - if (BOOL) then EXP1 else EXP2 [both expressions are of the same type]
LAM EXP - \IDN :: TYPE -> EXP e.g. "\x :: Int -> x + 1" is a function that increments it's passed integer value by 1.
APP EXP - EXP1 EXP2 perform EXP1 using EXP2 as it's argument e.g. "(\x :: Int -> x + 1) 2" returns the value 3.
LET EXP - let DEFNS+ in EXP, multiple DEFNs must be seperated by a semi-colon ';' but the final DEFN does not have one. 
          e.g. "let a = 1; b = 2; inc = \x :: Int -> x + 1 in (inc a) + b" will return 4.
```
