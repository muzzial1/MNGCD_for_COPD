#!/bin/bash
RANDOM=$1
nome_multiplex=$2
output_spec=$3
output_nospec=$4
MAXCOUNT=100
count=1


cd /uhd/username/Infomap/
RANDOM=$1
while [ "$count" -le $MAXCOUNT ]      # Generate 100 ($MAXCOUNT) random integers.
do 
	number=$RANDOM
  	./Infomap /uhd/username/prova_nmi/$output_nospec/multiplex/$nome_multiplex —-clu  -i multiplex -s$number —-silent --out-name $count  /uhd/username/prova_nmi/$output_nospec/outputinfomap/$output_spec		
  	let "count += 1"  # Increment count
done


            
 
