k=1000

mkdir database
# for type in {Rev,Trans}
# do
type=RevTrans
    mkdir database/${type}
    for op in {25,50,75,100}
    do
        mkdir database/${type}/$op
        for n in `seq 50 50 500`
        do
            stack exec DB -- -k $k -n $n -r $op -t $type -p 50 -o database/${type}/$op/${type}_R${op}_N${n}.txt
        done
    done
# done
