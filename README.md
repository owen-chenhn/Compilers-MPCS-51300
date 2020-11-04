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


For fuzz testing, intentional easy-to-find bugs include:  
1. Not able to handle recursive functions.  
2. Never initialize pointers of *all* the structs in their constructors.  
3. Never add function names to the function table when functions are defined.  
4. Never clear the variable table.   
