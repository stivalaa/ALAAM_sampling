#!/bin/sh
#
# Run a single IPNet estimation, using the template file and replacing
# @SESSION 
# @ATTRFILE
# with session name and attribute filename, the latter being the .clu
# file with attributes, with no Pajek header line
# 
# Usage: run_estimation.sh sessionName attributeFilename

IPNET=${HOME}/alaam/IPNet/IPNetv1.5/IPNet
TEMPLATE=./setting_attributes.template

if [ $# -ne 2 ]; then
  echo "Usage: $0 sessionName attibuteFilename" >&2
  exit 1
fi

oldcwd=$PWD
sessionName=$1
attrFilename=$2


tempdir=`mktemp -d`
SETTINGS_FILE=$tempdir/setting.txt
ESTIMATION_RESULTS_FILE=$tempdir/estimation-${sessionName}.txt
ATTR_FILE=${tempdir}/`basename ${attrFilename}`

tail -n+2 ${attrFilename} > $ATTR_FILE
nodes=`cat ${ATTR_FILE} | wc -l`
cat ${TEMPLATE} | sed "s!@SESSION!${sessionName}!;s!@ATTRFILE!${ATTR_FILE}!;s!@SAMPLENUM!${sessionName}!g;s!@VCOUNT!${nodes}!g" > ${SETTINGS_FILE}

cd ${tempdir} && time ${IPNET} ${SETTINGS_FILE}

cd ${oldcwd}
cp ${ESTIMATION_RESULTS_FILE} .

rm ${tempdir}/*
rmdir ${tempdir}

