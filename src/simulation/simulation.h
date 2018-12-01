//
// Created by wujiahao on 18-5-7.
//

#ifndef MHHSS_SIMULATION_H
#define MHHSS_SIMULATION_H


#include <mpi.h>
#include "../utils/predefine.h"
#include "../subbasin/simulation_subbasin.h"
#include "../context.h"
#include "subbasins_container.h"

class Simulation {

private:
    Simulation();

    static void initSimulation(int argc, char *argv[]);
    static void prepareSimulation();
    static void runSimulation();
    static void finishSimulation();


public:
    static SubbasinsContainer *subbasinsCt;
    static Context *context;

    static void run(int argc, char *argv[]);

#ifdef USE_PTHREAD
    static void *threadRecvMsgFun(void *sct);
#endif
};


#endif //MHHSS_SIMULATION_H
