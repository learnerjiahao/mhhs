#!/usr/bin/env bash

P=$1
COUNT=$2
D=$3

for i in $(seq 1 ${COUNT})
do

   echo "[dispatch]" > config/strong/config.toml_${P}_${D}
   echo "dispatch_file_prefix = \"dispatch/strong/dispatch_${P}_${D}/dispatch.json\"" >> config/strong/config.toml_${P}_${D}
   echo "[simulation]" >> config/strong/config.toml_${P}_${D}
   echo "simulation_steps = 100" >> config/strong/config.toml_${P}_${D}

   mkdir dispatch/strong/dispatch_${P}_${D} -p
   ../tool/create_json/ctdp -p ${P} -d ${D} -o dispatch/strong/dispatch_${P}_${D}/dispatch.json

   P=$[P*2]
done