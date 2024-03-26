#!/bin/bash
#set -xv
setperarr=$1
cd /uhd/username/prova_nmi/
countFile=maxcount.txt
if [ -e "$countFile" ];then
        for j in $(ls -1d output_EXA_SEV_*);do
		echo $j
		R --slave --file=/uhd/username/prova_nmi/Scripts/risultato_finale.R --args $j $setperarr
                sleep 10
        done
else
        echo "File Max count does not exist" 
fi  

