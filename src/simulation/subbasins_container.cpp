//
// Created by wujiahao on 18-5-11.
//

#include <sstream>
#include <cmath>
#include <algorithm>
#include "subbasins_container.h"
#include "../utils/mpiutil.h"

SubbasinsContainer *SubbasinsContainer::sc_instance = nullptr;
//std::map<utils::_type_nodeid, SimulationSubbasin> *SubbasinsContainer::subbasins
//        = new std::map<utils::_type_nodeid, SimulationSubbasin>();

SubbasinsContainer *SubbasinsContainer::getInstance(utils::_type_time_step allSimulationSteps) {
    if(sc_instance == nullptr)
        sc_instance = new SubbasinsContainer(allSimulationSteps);
    return sc_instance;
}

SubbasinsContainer::~SubbasinsContainer() {
    delete subbasins;
}

SubbasinsContainer::SubbasinsContainer(utils::_type_time_step allSimulationSteps) : allSimulationSteps(allSimulationSteps) {
    subbasins = new std::map<utils::_type_nodeid, SimulationSubbasin>();
}

bool SubbasinsContainer::checkOneStreamLocated(utils::_type_nodeid id) {
    std::map<utils::_type_nodeid, SimulationSubbasin>::iterator it = subbasins->find(id);
    return (it != subbasins->end());
}

void SubbasinsContainer::appendRoutingData(const TRoutingData &data) {

    if(!checkOneStreamLocated(data.getDownstreamId())) {
#ifdef DEV_MODE1
    std::stringstream ssMsg;
    ssMsg << "P" << mpiutil::proid << ": recv data whose target nodeid is "
                                      << data.getDownstreamId()
            << " timestep is " << data.getTimeStep() << " flow is " << data.getFlow() << std::endl;
    printf(ssMsg.str().c_str());
#endif
        std::stringstream sErrMsg;
        sErrMsg << "subbasin whose nodeId is " << data.getDownstreamId() << " is not distributed to this processor!!!";
        mpiutil::mpiAbort(sErrMsg.str(), -2, false);
    }
    SimulationSubbasin &targetSubbasin = subbasins->at(data.getDownstreamId());
    targetSubbasin.appendRoutingData(data);
}

bool SubbasinsContainer::tryAppendingRoutingData(TRoutingData &data, MPI_Request &request, int &recvFlag) {
    MPI_Test(&request, &recvFlag, MPI_STATUS_IGNORE);
    if(recvFlag == 0)
        return false;
#ifdef DEV_MODE1
    std::stringstream ssMsg;
    ssMsg << "P" << mpiutil::proid << "-----> tryAppendingRoutingData: recv data from "
              << recvStatus.MPI_SOURCE << " :::::"
              << " timestep:" << data.getTimeStep()
              << " nodeid:" << data.getDownstreamId()
              << " flow:" << data.getFlow()
              << std::endl;
    printf(ssMsg.str().c_str());
#endif
    appendRoutingData(data);
    return true;
}


SimulationSubbasin *SubbasinsContainer::pickAllSimulatable() {
    // todo more complex pick method
    std::map<utils::_type_nodeid, SimulationSubbasin>::iterator it;
    for(it = subbasins->begin(); it != subbasins->end(); it++) {
        if(it->second.isWaiting4Simulation(allSimulationSteps)) {
#ifdef DEV_MOD1
            std::stringstream ss;
            ss << mpiutil::proid << "------> pickAllSimulatable: picked nodeid " <<  it->first
                      << " at steptime " << it->second.getNowSimuStep() << std::endl;
            printf(ss.str().c_str());
#endif
            return &(it->second);
        }
    }
    return nullptr;
}

SimulationSubbasin *SubbasinsContainer::pickAllSimulatableComplex() {
    // todo more complex pick method
    for(auto itout = sortCt.begin(); itout != sortCt.end(); itout++) {
        for(auto itin : itout->second) {
            if(subbasins->at(itin).isWaiting4Simulation(allSimulationSteps)) {
#ifdef DEV_MOD1
                std::stringstream ss;
                ss << mpiutil::proid << "------> pickAllSimulatable: picked nodeid " <<  it->first
                      << " at steptime " << it->second.getNowSimuStep() << std::endl;
                printf(ss.str().c_str());
#endif
              return &(subbasins->at(itin));
            }
        }
    }
    return nullptr;
}

SimulationSubbasin *SubbasinsContainer::pickSlopeSimulatable() {
    // todo more complex pick method
    std::map<utils::_type_nodeid, SimulationSubbasin>::iterator it;
    for(it = subbasins->begin(); it != subbasins->end(); it++) {
        if(!it->second.isSlopeSimuedNowStep()) {
            return &(it->second);
        }
    }
    return nullptr;
}

bool SubbasinsContainer::isAllFinished() {
    std::map<utils::_type_nodeid, SimulationSubbasin>::iterator it;
    for(it = subbasins->begin(); it != subbasins->end(); it++) {
        if(!it->second.isCompleted(allSimulationSteps)) {
            return false;
        }
    }
    return true;
}

utils::_type_nodeid SubbasinsContainer::subbasinsSize() {
    return subbasins->size();
}

void SubbasinsContainer::addNewSubbasin(const utils::_type_nodeid nodeid, const SimulationSubbasin &subbasin) {
    subbasins->insert(std::pair<utils::_type_nodeid, SimulationSubbasin>(nodeid, subbasin));
}

SubbasinsContainer::SubbasinsContainer() {}

utils::_type_nodeid SubbasinsContainer::solveUpstreamsNeedComm() {
    int count = 0;
    for(auto subb : *subbasins) {
        count += subb.second.upstreamCountNotSameLocate();
    }
    return count;
}

void SubbasinsContainer::appendRoutingDatas(std::vector<TRoutingData> &tRoutingDatas) {

    for(auto tRoutingData : tRoutingDatas) {
#ifdef DEV_MODE1
        std::stringstream ss;
        ss << "----------------P" << mpiutil::proid <<  " :::::" << tRoutingData.getTimeStep()
              << "----" << tRoutingData.getDownstreamId() << "----" << tRoutingData.getFlow()
              << "\n";
        printf(ss.str().c_str());
#endif
        this->appendRoutingData(tRoutingData);
    }
}

unsigned long SubbasinsContainer::getAllCommucationCount() {
    unsigned long count = 0;
    for(auto subbasin : *subbasins) {
        count += subbasin.second.upstreamCountNotSameLocate();
    }
    return count*this->allSimulationSteps;
}

bool SubbasinsContainer::compare4SortPick(const utils::_type_nodeid &first,
                                          const utils::_type_nodeid &second) {
    // todo the first field should be compared may be the depth of the depths of the two subbasins
    if (ceil(sqrt(sc_instance->subbasins->at(first).getNodeid())) > ceil(sqrt(sc_instance->subbasins->at(second).getNodeid()))) {
        return true;    // return true meanings don't change the location of first and second
    }

    // todo the second field should be compared may be the downstreams Count of the two subbasins
    if(sc_instance->subbasins->at(first).downstreamCountNotSameLocate() > sc_instance->subbasins->at(second).downstreamCountNotSameLocate()) {
        return true;
    }

    // todo the second field should be compared may be the depth form the src node
    if(sc_instance->subbasins->at(first).getDepth() < sc_instance->subbasins->at(second).getDepth()) {
        return true;
    }

    return false;
}

void SubbasinsContainer::sortCt4PickingSimu(utils::_type_time_step timeStep) {
    this->sortCt.at(timeStep).sort(compare4SortPick);
}

void SubbasinsContainer::initSortCt() {
    utils::_type_time_step startSimuTimestep = 1;
    std::list<utils::_type_time_step> tmpList;
    sortCt.insert(std::make_pair(startSimuTimestep, tmpList));
    for (auto subb : *subbasins) {
        sortCt.at(startSimuTimestep).push_back(subb.first);
    }
    sortCt.at(startSimuTimestep).sort(compare4SortPick);
}

void SubbasinsContainer::updateSortCt(utils::_type_time_step timestep, utils::_type_nodeid nodeid) {
    auto findIt = this->sortCt.find(timestep + 1);
    if(findIt == this->sortCt.end()) {
        std::list<utils::_type_nodeid> tmpList;
        this->sortCt.insert(std::make_pair(timestep + 1, tmpList));
    }
    this->sortCt.at(timestep).remove(nodeid);    // remove the old
    this->sortCt.at(timestep + 1).push_back(nodeid);  // insert the new
    this->sortCt.at(timestep + 1).sort(compare4SortPick);
    // todo delete this->sortCt.at(timestep).size() == 0
}

