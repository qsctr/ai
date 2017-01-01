# Semester 1 final project

See the source code on GitHub: https://github.com/qsctr/javascript-optimizer

My project is a program which checks JavaScript programs for errors and optimizes them. This is related to this course because I have to traverse the AST (abstract syntax tree) of JavaScript programs. And since it is a tree I can use search methods we learned such as depth-first search.

I didn't have a lot of time to work on this project, because of the VEX robotics competition, so there isn't that much stuff my program can do, but new stuff can easily be added.

## 11/22/2016

Today I found a library for parsing JavaScript into an abstract syntax tree. I also wrote a very basic function to remove empty statements in the syntax tree. Empty statements are basically extra semicolons in the program that are useless.

## 11/24/2016

Today I added a function to remove useless return statements. For example, if the return statement is at the end of the function, and it is returning `undefined`.

## 11/28/2016

Today I added stuff to replace the void operator with undefined and detecting return statements outside of a function.

## 12/4/2016

Today I added functions to remove `debugger;` statements, and to remove unused functions. I am not removing top-level functions since they are in the global scope and might get called from other files, if the script is run in a browser.

## 12/5/2016

Today I added a function to detect variable shadowing, which is when a variable (or parameter or function) in an inner scope has the same name as a variable declared in some outer scope. This is bad because code inside the inner scope cannot reference the outer variable, and it also makes your code more confusing. I also added a function to remove unused variable declarations. It also does not apply to top-level declarations.