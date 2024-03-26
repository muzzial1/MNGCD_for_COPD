#!/bin/bash
#SBATCH --mail-user=username@gsk.com
#SBATCH --mail-type=END
#SBATCH --requeue
#SBATCH --job-name=Test
#SBATCH --array=1-20


A=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20")
nice -n 19 R '--vanilla' '--slave' --file=/uhd/username/prova_nmi/Scripts/salvoxlconsensi_cal.R --args ${A[$SLURM_ARRAY_TASK_ID-1]} $1 $2

