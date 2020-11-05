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


For fuzz testing, one easy-to-find bug:  
1. Not able to handle recursive functions. The compiler will erroneously complain when input program contains recursive function (like function "fib" in test/test1.ek).  
