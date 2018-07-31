//
// Created by wujiahao on 18-5-10.
//

#include <sstream>
#include "routed_data_imsgs_pool.h"
#include "../simulation/trouting_data.h"
#include "../utils/mpiutil.h"

RoutedDataIMsgsPool::RoutedDataIMsgsPool(utils::_type_nodeid initCap) : initCap(initCap) {

    this->msgsPool = new std::vector<RoutedDataIMessage *>();
    for (int i = 0; i < initCap; ++i) {
        this->msgsPool->push_back(new RoutedDataIMessage());
    }
}

RoutedDataIMsgsPool::~RoutedDataIMsgsPool() {
    for (int i = 0; i < this->msgsPool->size(); ++i) {
        delete (this->msgsPool->at(i));
    }
    delete (this->msgsPool);
}

void RoutedDataIMsgsPool::chooseAllAvail2IrecvAndSaveFinNoWait(std::vector<TRoutingData> &hadRecvedDatas) {

    unsigned int i = 0;

    for (; i < this->msgsPool->size(); i++) {
        if (this->msgsPool->at(i)->isAvailable()) {
            this->msgsPool->at(i)->msgIrecv();
        } else {
            int flag = 1;
            MPI_Test(this->msgsPool->at(i)->getRequestTag(), &flag, MPI_STATUS_IGNORE);
            if (flag != 0) {
                hadRecvedDatas.push_back(this->msgsPool->at(i)->getTRoutingData());
                this->msgsPool->at(i)->makeAvailable();
            }
        }
    }
}

void RoutedDataIMsgsPool::chooseAllAvail2IrecvAndSaveFinNoWaitButDy(std::vector<TRoutingData> &hadRecvedDatas) {

    unsigned int i = 0, unavailableCount = 0;

    for (; i < this->msgsPool->size(); i++) {
        if (this->msgsPool->at(i)->isAvailable()) {
            this->msgsPool->at(i)->msgIrecv();
        } else {
            int flag = 1;
            MPI_Test(this->msgsPool->at(i)->getRequestTag(), &flag, MPI_STATUS_IGNORE);
            if (flag != 0) {
                hadRecvedDatas.push_back(this->msgsPool->at(i)->getTRoutingData());
                this->msgsPool->at(i)->makeAvailable();
            } else {
                unavailableCount ++;
            }
        }
    }

    if (unavailableCount == this->msgsPool->size()) {
        int msgComeFlag;
        MPI_Iprobe(MPI_ANY_SOURCE, mpiutil::proid, MPI_COMM_WORLD, &msgComeFlag, MPI_STATUS_IGNORE);
        if (msgComeFlag != 0) {
            this->msgsPool->push_back(new RoutedDataIMessage());
            this->msgsPool->at(i)->msgIrecv();
        }
    }

    // todo clear index > 2 * initsize 's finished commucation
}
void RoutedDataIMsgsPool::chooseAllAvail2IrecvAndSaveFinMayWait(std::vector<TRoutingData> &hadRecvedDatas) {
    unsigned int i = 0;
    std::vector<MPI_Request> unAvailablesReqTags;

    for (; i < this->msgsPool->size(); i++) {
        if (this->msgsPool->at(i)->isAvailable()) {
            this->msgsPool->at(i)->msgIrecv();
        } else {
            int flag = 1;
            MPI_Test(this->msgsPool->at(i)->getRequestTag(), &flag, MPI_STATUS_IGNORE);
            if (flag != 0) {
                hadRecvedDatas.push_back(this->msgsPool->at(i)->getTRoutingData());
                this->msgsPool->at(i)->makeAvailable();
            } else {
                unAvailablesReqTags.push_back(*(this->msgsPool->at(i)->getRequestTag()));
            }
        }
    }

    if (unAvailablesReqTags.size() == this->msgsPool->size()) {
        int finishIndex = -1;
        MPI_Waitany(this->msgsPool->size(), unAvailablesReqTags.data(), &finishIndex, MPI_STATUS_IGNORE);
        hadRecvedDatas.push_back(this->msgsPool->at(finishIndex)->getTRoutingData());
        this->msgsPool->at(finishIndex)->msgIrecv();
    }
}


void RoutedDataIMsgsPool::chooseAvailableAndIsendNoWait(utils::_type_proid destProid, const TRoutingData &data) {

    unsigned int i = 0;

    for (; i < this->msgsPool->size(); i++) {

        if (this->msgsPool->at(i)->isAvailable()) {
            this->msgsPool->at(i)->msgIsend(destProid, data);
            return;
        }

        int flag = 1;
        MPI_Test(this->msgsPool->at(i)->getRequestTag(), &flag, MPI_STATUS_IGNORE);
        if (flag != 0) {
            this->msgsPool->at(i)->msgIsend(destProid, data);
            return;
        }
    }

    this->msgsPool->push_back(new RoutedDataIMessage());
    this->msgsPool->at(i)->msgIsend(destProid, data);
}

void RoutedDataIMsgsPool::chooseAvailableAndIsendMayWait(utils::_type_proid destProid, const TRoutingData &data) {

    std::vector<MPI_Request> unAvailablesReqTags;
    unsigned int i = 0;

    for (; i < this->msgsPool->size(); i++) {

        if (this->msgsPool->at(i)->isAvailable()) {
            this->msgsPool->at(i)->msgIsend(destProid, data);
            return;
        }

        int flag = 1;
        MPI_Test(this->msgsPool->at(i)->getRequestTag(), &flag, MPI_STATUS_IGNORE);
        if (flag != 0) {
            this->msgsPool->at(i)->msgIsend(destProid, data);
            return;
        }
        unAvailablesReqTags.push_back(*(this->msgsPool->at(i)->getRequestTag()));
    }

    int finishIndex = -1;
    MPI_Waitany(this->msgsPool->size(), unAvailablesReqTags.data(), &finishIndex, MPI_STATUS_IGNORE);
    this->msgsPool->at(finishIndex)->msgIsend(destProid, data);

}
