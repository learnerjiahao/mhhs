//
// Created by wujiahao on 18-5-9.
//

#include "mpiutil.h"

utils::_type_proid mpiutil::proRanks = 0;
utils::_type_proid mpiutil::proid = 0;
const utils::_type_proid mpiutil::masterProid = 0;
const int mpiutil::MPI_ROUTING_MSG_TAG = 1;


void mpiutil::mpiInit(int argc, char *argv[]) {

//    MPI_Init(&argc, &argv);
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, nullptr);
    MPI_Comm_size(MPI_COMM_WORLD, reinterpret_cast<int *>(&proRanks));
    MPI_Comm_rank(MPI_COMM_WORLD, reinterpret_cast<int *>(&proid));

}


void mpiutil::mpiFinish() {
    MPI_Finalize();
}

void mpiutil::mpiAbort(std::string errMsg, int errCode, bool onlyMasterP) {
    if(onlyMasterP) {
        if(proid == masterProid) {
            std::cerr << errMsg  << std::endl;
        }
    } else {
        std::cerr << "P" << proid << ": " << errMsg << std::endl;
    }
//    MPI_Finalize();
//    exit(errCode);
#ifdef USE_MPI
    MPI_Abort(MPI_COMM_WORLD, errCode);
#else
    exit(errCode);
#endif

}
//
//template<typename T>
//void mpiutil::mpiIRecvAnyWorld(T &data, int tag, MPI_Request &request) {
//    MPI_Irecv(&data, sizeof(T), MPI_BYTE, MPI_ANY_SOURCE, MPI_ROUTING_MSG_TAG, MPI_COMM_WORLD, &request);
//}
//
//template<typename T>
//void mpiutil::mpiISendWorld(T &data, int dest, int tag, MPI_Request &request) {
//    MPI_Isend(&data, sizeof(T), MPI_BYTE, dest, tag, MPI_COMM_WORLD, &request);
//}
