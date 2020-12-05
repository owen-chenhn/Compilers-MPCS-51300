declare -a opt_str_arr=("" 
                        "-funcinline" 
                        "-instcmb" 
                        "-reassoc" 
                        "-cfgsimp" 
                        "-funcinline -instcmb -reassoc -cfgsimp")

declare -a files_str_arr=("inline.ek" "inst_cmb.ek" "reass.ek" "cfg_simp.ek" "all.ek")                    

for i in "${opt_str_arr[@]}"
do 
    echo "##################################"
    echo "optimizations: " "$i" 
    for j in "${files_str_arr[@]}"
    do 
        echo "      file: " "$j"
        ./bin/ekcc -O -jit $i -t ./test_files/$j 
    done 
done 