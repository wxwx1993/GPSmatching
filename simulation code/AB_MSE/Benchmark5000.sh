#!/bin/bash

#SBATCH -n 16 #number of cores
#SBATCH -t 3-23:00
#SBATCH -o /n/home01/xwu1993/BCDOT/test_error.out #specify where to save errors returned by the program
#SBATCH -e /n/home01/xwu1993/BCDOT/test_log.err #specify where to save the output log
#SBATCH --array=1-7 #number of jobs to run, it is currently set to 1 job(1 year), change it to array=1-13 for 13 years(jobs)
#SBATCH --mem=200000 #memory requested
#SBATCH -J gps5000  #job name, this case:aw-area weighted aggregation
#SBATCH --mail-type=ALL #notifications for job done
#SBATCH --mail-user=wuxiao@g.harvard.edu # send to address

export R_LIBS_USER=/n/home01/xwu1993/apps/R_3.4.2:$R_LIBS_USER  #change it accordingly
module load R/3.4.2-fasrc01
#module load R_core/3.4.2-fasrc01
#module load R_packages/3.4.2-fasrc02

R CMD BATCH --quiet --no-restore --no-save Benchmark5000.R /n/home01/xwu1993/GPSmatching/output/test_${SLURM_ARRAY_TASK_ID}.Rout
