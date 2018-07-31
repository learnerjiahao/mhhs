#!/usr/bin/env bash

P=$1
DMIN=$2
DMAX=$3


for D in $(seq ${DMIN} ${DMAX})
do
    for i in $(seq 1 100)
    do
        "Time ${i}:" >> outputs/n_build/1_build_${P}_${D}
        mpirun -n ${P} ../1_np_nn_build/bin/mhhss -c config/n_build/config.toml_${P}_${D} >> outputs/n_build/1_build_${P}_${D}
        "Time ${i}:" >> outputs/n_build/2_build_${P}_${D}
        mpirun -n ${P} ../2_p_bsbr_build/bin/mhhss -c config/n_build/config.toml_${P}_${D} >> outputs/n_build/2_build_${P}_${D}
        "Time ${i}:" >> outputs/n_build/3_build_${P}_${D}
        mpirun -n ${P} ../3_p_bshr_build/bin/mhhss -c config/n_build/config.toml_${P}_${D} >> outputs/n_build/3_build_${P}_${D}
        "Time ${i}:" >> outputs/n_build/4_build_${P}_${D}
        mpirun -n ${P} ../4_p_nsbr_build/bin/mhhss -c config/n_build/config.toml_${P}_${D} >> outputs/n_build/4_build_${P}_${D}
        "Time ${i}:" >> outputs/n_build/5_build_${P}_${D}
        mpirun -n ${P} ../5_p_nshr_build/bin/mhhss -c config/n_build/config.toml_${P}_${D} >> outputs/n_build/5_build_${P}_${D}
        "Time ${i}:" >> outputs/n_build/6_build_${P}_${D}
        mpirun -n ${P} ../6_p_hsbr_build/bin/mhhss -c config/n_build/config.toml_${P}_${D} >> outputs/n_build/6_build_${P}_${D}
        "Time ${i}:" >> outputs/n_build/7_build_${P}_${D}
        mpirun -n ${P} ../7_p_hshr_build/bin/mhhss -c config/n_build/config.toml_${P}_${D} >> outputs/n_build/7_build_${P}_${D}
    done
done