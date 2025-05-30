#!/bin/bash

#SBATCH -A desi
#SBATCH -C cpu
#SBATCH --qos=regular
#SBATCH --time=04:30:00
#SBATCH --nodes=1
#SBATCH --output=log/JOB_OUT_%x_%j.txt
#SBATCH --error=log/JOB_ERR_%x_%j.txt

###export OMP_NUM_THREADS=2

#first steps, get environment

srun -N 1 -u python /global/cfs/cdirs/desi/survey/catalogs/main/mocks/FAemu_preliminary/sikandar/Updated_Code_CFC/fof_v1.0/scriptfofAb.py