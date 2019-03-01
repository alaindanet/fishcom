#!/bin/bash
#PBS -l select=1:ncpus=1
#PBS -l walltime=60:00:00
#PBS -N mytestjob
#PBS -j oe
echo "It works" > result.txt

