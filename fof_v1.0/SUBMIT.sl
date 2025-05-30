#!/bin/bash -l

##SBATCH --qos=debug
##SBATCH --time=00:02:00

#SBATCH --qos=regular
#SBATCH --time=4:00:00

#SBATCH -A desi
#SBATCH -C cpu
#SBATCH -q shared
#SBATCH -n 1
##SBATCH -c 32
##SBATCH --nodes=1
##SBATCH --tasks-per-node=1
##SBATCH --mem=10000
#SBATCH --cpus-per-task=1
#SBATCH --job-name=fof
#SBATCH -o log/%x_%j.o
#SBATCH -e log/%x_%j.e

##export OMP_NUM_THREADS=1

rootDIR=/global/cfs/cdirs/desi/survey/catalogs/main/mocks/FAemu_preliminary/sikandar/Updated_Code_CFC/fof_v1.0
xFILE=/fof_lightcone_ang
pFILE=INI_fof_lightcone_ang.txt

cd $SLURM_SUBMIT_DIR

$rootDIR$xFILE $pFILE