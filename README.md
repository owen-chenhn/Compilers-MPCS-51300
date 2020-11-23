# Compilers-MPCS-51300
Coding assignments of the course MPCS-51300-2020Fall Compilers. 

Members:  
Haoning Chen (owenchen97@uchicago.edu)  
Yi Ding (yiding1@uchicago.edu)  


Instruction:
For assignment 1, please 
1. cd to Compilers-MPCS-51300/ 
2. run 'make'
3. run ./bin/ekcc -emit-ast -o <output_file> <input_file>

For assignment 2, 
Deficits:
1. We have implemented the check for function call before declaration. But at this point this implementation would result in recursive functions not being able to process successfully. E.g., the function "fib" calls itself in its function body, and our compiler will report error about this, which is a minor bug of our implementation, because we tried to do all the error checking in parsing stage (i.e., during construction of each symbol structure). However, for truely error cases (like calling a truely undefined function), our compiler can correctly detect them.  

Bonus:
1. We implemented duplicate variable declaration check.
2. Checked variable usage before declaration.
3. Checked whether passed in function parameters match function signature. 
4. Checked whether the return type of a function indeed matches the type of all the return statements in its function body.  


For assignment 3, 

We assume llvm-config and clang version of 7.0 in the environment for successful make. 

After make, you can run ./bin/ekcc -emit-llvm -o <output_file> <input_file> 
and get the IR(somewhat a .ll) <output_file>. 

Or run ./bin/ekcc -o <output_file> <input_file> and get the <output_file> executable.


We currently still have some issues linking the getarg functions in customed library. The program can still run, starting from run(), and print.  


For assignment 4,  

We have fixed all the deficits mentioned above. Optimization is added.  

Note: for now we haven't implemented **JIT** execution yet. Please statically compile input program and run the executable.  
