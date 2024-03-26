#!/bin/bash
#set -xv
cd /working_dir/
countFile=maxcount.txt
if [ -e "$countFile" ];then
        for j in $(ls -1d output_CURRCOPD_*Very*);do
                while [ $(squeue|grep -c username) -ge  $(cat maxcount.txt) ];do
                        echo "Waiting for 30 seconds: "'squeue|grep -c username'" jobs in the queue."
                        sleep 30
                done
		echo $j
                sbatch /uhd/username/prova_nmi/Scripts/perparcopy2.sh $j
                sleep 300
        done
else
        echo "File Max count does not exist" 
fi  

