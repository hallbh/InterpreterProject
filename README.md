# InterpreterProject
An implementation of an interpreter in Petite Chez Scheme for Programming Language Concepts

This interpreter assumes that exactly one argument is passed to a lambda expression. Adding more variables
can be achieved by currying the lambdas, and a function of no arguments can be passed an arbitrary value for an arbitrary parameter.

The goal of this project is to write an interpreter for C in Scheme. We started by writing a basic interpreter
in scheme, and converting it into Continuation Passing Style, to ensure potentailly unbounded recursion does not
cause a stack overflow. The file P01.ss contains the CPS-ed interpreter as well as a function called lex, which converts
a scheme program into a lexed version of the progam, where bound variables are replaced by their lexical addresses
and lambda parameters are dropped. The interpreter matches on the lexed version of a program to determine its value.

The second milestone of this project was to make the interpreter representation independent with respect to continuations, environment,
and closures, and remove all high-order functions. The file P02.ss represents these changes. Note that constructors exist for creating
continuations, environments. These constructors build lists of variable values for each level of the environment, which are matched on
in the apply-[] functions. 

The third milestone is in progress, and not all course material has been covered at this point. This is subject to change and accurate 
as of 1/31/18.
