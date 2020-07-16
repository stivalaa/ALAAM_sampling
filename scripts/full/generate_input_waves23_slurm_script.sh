#!/bin/bash

#SBATCH --job-name="generate_input_waves23"
#SBATCH --nodes=1
#SBATCH --time=0-8:00:00
#SBATCH --mem=16G

#
# File:    generate_input.sh
# Author:  Alex Stivala
# Created: September 2015
#
# generate_input.sh - build directories for IPNet estimations
#
# Usage: geneate_input.sh
#
# The folmediuming variables are replaced:
#   @ALAAM
#   @WAVES
#   @SEEDS
# in the folmediuming template files:
#   run_all_estimations_pbs_script.template
#   setting_attributes.template
#

alaam=${PROJECTDIR}/Alaam_estimations_project90_simulated_contagion0

PBS_TEMPLATE=./run_all_estimations_slurm_script.template
SETTINGS_TEMPLATE=./setting_attributes.template


waves="2 3"
seeds=`seq 1 20`

for wave in ${waves}
do
  for seed in ${seeds}
  do
    outdir=./estimation_project90_waves${wave}_seeds${seed}
    mkdir ${outdir}
    cat ${SETTINGS_TEMPLATE} | sed "s!@ALAAM!${alaam}!g;s!@WAVES!${wave}!g;s!@SEEDS!${seed}!g" > ${outdir}/setting_attributes.template
    cat ${PBS_TEMPLATE} | sed "s!@ALAAM!${alaam}!g;s!@WAVES!${wave}!g;s!@SEEDS!${seed}!g" > ${outdir}/run_all_estimations_pbs_script.sh

    (cd ${outdir} && Rscript ${HOME}/alaam/scripts/snowballSampleFull.R  ${wave} ${seed} ${HOME}/alaam/simulated_Project90_contagion0/project90_giantcomponent_adjmatrix.txt ${HOME}/alaam/simulated_Project90_contagion0/binaryAttribute_50_50_n4430.txt  ${HOME}/alaam/simulated_Project90_contagion0/continuousAttributes_n4430.txt  ${HOME}/alaam/simulated_Project90_contagion0/sample-project90\*.clu)
  done
done

