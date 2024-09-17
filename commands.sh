#!/bin/bash
set -euxo pipefail

# define some stuff: here is the latest SMARTER dataset you can get from webserver.ibba.cnr.it FTP,
# set the basename file like plink does:
SMARTER_DATASET="SMARTER-OA-OAR3-top-0.4.3.dev0"

# those are the plink options which generated the SMARTER sheep data. the '--sheep' 
# plink option is an alias for all of these
SHEEP_OPTS="--chr-set 26 no-xy --allow-extra-chr"

# get rid of unwanted SNPs and SAMPLES from smarter dataset
plink ${SHEEP_OPTS} --bfile ${SMARTER_DATASET} --keep hapmap_samples.txt --extract snp_list.txt --out ${SMARTER_DATASET}-subset --make-bed
