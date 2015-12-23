#!/bin/bash

# This script is to make multiple copies of the model to run on various nodes of a cluster

dir="/local-scratch/$USER/yuk-unif"

mkdir $dir

rm -r $dir/*-run;

for i in {1..100};


do

  mkdir -p $dir/$i-run;

 cp isasugra.x $dir/$i-run

 cp job.condor $dir/$i-run

 cp rnd-pts-yu.txt $dir/

 echo $i > $dir/$i-run/run-pt.txt

done
