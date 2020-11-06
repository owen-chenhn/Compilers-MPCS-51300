# Compilers-MPCS-51300
Coding assignments of the course MPCS-51300-2020Fall Compilers. 

Members:  
Haoning Chen (owenchen97@uchicago.edu)  
Yi Ding (yiding1@uchicago.edu)  


Instruction:  

For fuzz testing:
1. Make sure to specify the right CXX value (clang++) before testing, assuming the correct clang+llvm has already been downloaded. 
e.g. export CXX=<somepath>/clang+llvm-7.0.0-x86_64-linux-gnu-ubuntu-16.04/bin/clang++ 

2. One bug that is easy to find is that the program is not able to handle recursive functions. The compiler will erroneously complain when input program contains recursive function (like function "fib" in test1.ek).  

3. After you run 'make', and then './bin/ekcc test1.ek' you will find the expected fuzz test result of reporting "error: Function should not return a reference." 

