#!/bin/bash
RANDOM=$1
nome_multiplex=$2
output_nspec=$3
MAXCOUNT=100
count=1


cd /uhd/username/Infomap/

while [ "$count" -le $MAXCOUNT ]      
do
  number=$RANDOM
  ./Infomap /uhd/username/prova_nmi/$output_nspec/perconsenso/$nome_multiplex/grafoc.txt -i link-list -s$number --out-name $count  /uhd/username/prova_nmi/$output_nspec/perconsenso/$nome_multiplex/outputperconsenso/  
  let "count+= 1"  # Increment count
done
