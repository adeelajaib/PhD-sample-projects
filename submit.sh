#!/bin/bash

# This script goes into each folder and submits jobs to the cluster

cd /local-scratch/$USER/yuk-unif;

for i in $(ls -d *-run | sort -n); 

#do pwd;
do cd $i;

# qsub -N 1-RUN -q standby -l procs=1,walltime=00:5:00 -A TG-PHY120014 scripts.sh

condor_submit job.condor

sleep 2;

#tg_short
#tg_workq

cd ../;
done


