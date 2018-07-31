#!/usr/bin/env bash

P=$1
DMIN=$2
DMAX=$3

for D in $(seq ${DMIN} ${DMAX})
do
   echo "[dispatch]" > config/n_build/config.toml_${P}_${D}
   echo "dispatch_file_prefix = \"dispatch/n_build/dispatch_${P}_${D}/dispatch.json\"" >> config/n_build/config.toml_${P}_${D}
   echo "[simulation]" >> config/n_build/config.toml_${P}_${D}
   echo "simulation_steps = 100" >> config/n_build/config.toml_${P}_${D}

   mkdir dispatch/n_build/dispatch_${P}_${D} -p
   ../tool/create_json/ctdp -p ${P} -d ${D} -o dispatch/n_build/dispatch_${P}_${D}/dispatch.json

done