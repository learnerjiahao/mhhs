//
// Created by wujiahao on 18-5-7.
//

#ifndef MHHSS_MPIUTIL_H
#define MHHSS_MPIUTIL_H

#include <string>
#include "mpi.h"
#include <iostream>
#include "predefine.h"

//template<typename T>
class mpiutil {

public:
    static utils::_type_proid proRanks;
    static utils::_type_proid proid;
    static const utils::_type_proid masterProid;
    static const int MPI_ROUTING_MSG_TAG;

    static void mpiInit(int argc, char *argv[]);
    static void mpiFinish();
    static void mpiAbort(std::string errMsg, int errCode, bool onlyMasterP);

//    template<typename T>
//    static void mpiIRecvAnyWorld(T &data, int tag, MPI_Request &request);
//
//    template<typename T>
//    static void mpiISendWorld(T &data, int dest, int tag, MPI_Request &request);

};

#endif //MHHSS_MPIUTIL_H
