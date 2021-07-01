k=1000
op=25

mkdir database
for type in {Rev,Trans,Swap,Insertion}
do
    mkdir database/${type}
    for n in `seq 50 50 500`
    do
        stack exec DB -- -k $k -n $n -r $op -t $type -o database/${type}/${type}_R${op}_N${n}.txt
    done
done
