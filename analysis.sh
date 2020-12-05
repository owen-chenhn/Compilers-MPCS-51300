declare -a opt_str_arr=("" 
                        "-funcinline" 
                        "-instcmb" 
                        "-reassoc" 
                        "-cfgsimp" 
                        "-sroa" 
                        "-funcinline -instcmb -reassoc -cfgsimp -sroa")

declare -a files_str_arr=("test1.ek" "test2.ek" "test3.ek" "test4.ek")                    

for i in "${opt_str_arr[@]}"
do 
    echo "##################################"
    echo "optimizations: " "$i" 
    for j in "${files_str_arr[@]}"
    do 
        echo "      file: " "$j"
        ./bin/ekcc -O -jit $i -t ../test/$j 
    done 
done 