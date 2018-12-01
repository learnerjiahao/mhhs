//
// Created by wujiahao on 18-5-6.
//

#include <iostream>
#include <sstream>
#include <algorithm>
//#include <omp.h>
#include "simulation_subbasin.h"
#include "../utils/mpiutil.h"
#include "../context.h"
#include "../simulation/simulation.h"

bool SimulationSubbasin::isCompleted(utils::_type_time_step allSimulationSteps) {
    return this->nowSimulationStep > allSimulationSteps;
}

SimulationSubbasin::SimulationSubbasin() : nowSimulationStep(1), isSlopeSimuedThisStep(false) {
#ifdef USE_PTHREAD
    pthread_rwlock_init(&this->pthread_rwlock_upstreamsRoutedD, nullptr);
#endif
}

void SimulationSubbasin::afterOneStepSimu() {
#ifdef DEV_MODE1
    if(!isHeadwater()) {
        std::stringstream ssmsg;
        ssmsg << "P" << mpiutil::proid
              << " nodeid" << nodeid
              << " ::: nowtimestep=" << nowSimulationStep
              << " accntest=" << routingDataAcc.getNtest()
              << " \tntest=" << this->upstreamsRoutedD.at(this->nowSimulationStep).getNtest()
              << " isRouted=" << this->upstreamsRoutedD.at(this->nowSimulationStep).isAllRouted()
              << "\n";
        printf(ssmsg.str().c_str());
    }
#endif
    this->handleHadOutputDatas();
    this->deleteRoutedFinishedData();
    this->nowSimulationStep++;
    isSlopeSimuedThisStep = false;
}


void SimulationSubbasin::appendRoutingData(const TRoutingData &tRoutingData) {
#ifdef USE_PTHREAD
    pthread_rwlock_wrlock(&this->pthread_rwlock_upstreamsRoutedD);
#endif
    std::map<utils::_type_time_step, RoutedData>::iterator it
            = this->upstreamsRoutedD.find(tRoutingData.getTimeStep());
    if (it == this->upstreamsRoutedD.end()) {
        // todo this->upstreams.size() is equal to zero
        RoutedData routedData(this->upstreams.size() - 1);
        routedData.updateMetaData(tRoutingData);
        this->upstreamsRoutedD.insert(
                std::pair<utils::_type_time_step, RoutedData>(tRoutingData.getTimeStep(), routedData));
    } else {
        this->upstreamsRoutedD.at(tRoutingData.getTimeStep()).routing(tRoutingData);
    }
#ifdef USE_PTHREAD
    pthread_rwlock_unlock(&this->pthread_rwlock_upstreamsRoutedD);
#endif

#ifdef DEV_MODE1
    std::stringstream ssmsg;
    ssmsg << "P" << mpiutil::proid
                 << " nodeid" << nodeid
                 << " :nowtimestep=" << tRoutingData.getTimeStep()
                 << " flow=" << tRoutingData.getFlow()
                 << " ntest=" << tRoutingData.getNtest()
                 << "\n";

    for (auto data : upstreamsRoutedD) {
        ssmsg << "\t"
              << " " << data.first
              << " " << data.second.getNtest();
//              << " " << data.second.isAllRouted()
    }
    ssmsg << std::endl;
    printf(ssmsg.str().c_str());
#endif
}

bool SimulationSubbasin::isWaiting4Simulation(utils::_type_time_step allSimulationSteps) {

    if (isCompleted(allSimulationSteps)) {
        return false;
    }
    // whether upstreamsRoutedD[nowSimulationStep] is existed ==> upstreams.size() > 0
    if (isHeadwater())
        return true;

#ifdef USE_PTHREAD
    pthread_rwlock_rdlock(&this->pthread_rwlock_upstreamsRoutedD);
#endif
    std::map<utils::_type_time_step, RoutedData>::iterator itFind = upstreamsRoutedD.find(nowSimulationStep);
    if (itFind == upstreamsRoutedD.end()) {
#ifdef USE_PTHREAD
        pthread_rwlock_unlock(&this->pthread_rwlock_upstreamsRoutedD);
#endif
        return false;
    }
    bool isAllRoutedTag = upstreamsRoutedD.at(nowSimulationStep).isAllRouted();
#ifdef USE_PTHREAD
    pthread_rwlock_unlock(&this->pthread_rwlock_upstreamsRoutedD);
#endif
    return isAllRoutedTag;
}

bool SimulationSubbasin::isSlopeSimuedNowStep() {
    return this->isSlopeSimuedThisStep;
}

utils::_type_time_step SimulationSubbasin::getNowSimuStep() {
    return this->nowSimulationStep;
}

void SimulationSubbasin::slopePLSimulation() {
//    Simulation::hydroModels->at(nodeid)->
//            runProduceFlowSimul(Simulation::hydroModels->at(nodeid)->getOneSteptimeInputDatas(nowSimulationStep),
//                                nowSimulationStep); // todo
    RoutingDataMeta upRoutedDatas;
    this->nowFolwData = pRunoffModel->runModel(*pModelContext, *Config::getInstance(), upRoutedDatas, nowSimulationStep);
    isSlopeSimuedThisStep = true;
}


void SimulationSubbasin::routingInSimulation() {
#ifdef USE_PTHREAD
    pthread_rwlock_rdlock(&this->pthread_rwlock_upstreamsRoutedD);
#endif
    RoutingDataMeta upRoutedDatas;
    upRoutedDatas.setFlow(upstreamsRoutedD.at(this->nowSimulationStep).getFlow());
#ifdef USE_PTHREAD
    pthread_rwlock_unlock(&this->pthread_rwlock_upstreamsRoutedD);
#endif
//    this->nowFolwData = Simulation::hydroModels->at(nodeid)->
//            runRouteFlowSimul(Simulation::hydroModels->at(nodeid)->getOneSteptimeInputDatas(nowSimulationStep),
//                              nowSimulationStep, upRoutedDatas);
    this->nowFolwData = pRoutingModel->runModel(*pModelContext, *Config::getInstance(), upRoutedDatas, nowSimulationStep);
}


void SimulationSubbasin::routingOutSimulation() {
    if (this->routingData2Downstream.size() == 0) {
        for (auto downstream : this->downstreams) {
            RoutingDataMeta dataMeta;
            routingData2Downstream.
                    insert(std::pair<utils::_type_nodeid, RoutingDataMeta>(downstream.first, dataMeta));
        }
    }
    for (auto downstream : this->downstreams) {
        routingData2Downstream.at(downstream.first).setFlow(this->nowFolwData.getFlow()/routingData2Downstream.size());        // todo truely routingOutSimulation
    }
}

const std::map<utils::_type_nodeid, RoutingDataMeta> &SimulationSubbasin::runOneSimulation() {

    if (!this->isSlopeSimuedThisStep)
        slopePLSimulation();

    if (!isHeadwater())
        this->routingInSimulation();

    if (!isOutlet())
        this->routingOutSimulation();

#ifdef DEV_MODE1
    std::stringstream ss;
    ss << "P" << mpiutil::proid <<  "::"
       << " nodeid:" << this->nodeid
       << " step:" << this->nowSimulationStep
       << std::endl;
    printf(ss.str().c_str());
#endif
    this->afterOneStepSimu();

    return this->routingData2Downstream;
}

void SimulationSubbasin::deleteRoutedFinishedData() {
    // todo do not erase one at one time but more
#ifdef USE_PTHREAD
    pthread_rwlock_wrlock(&this->pthread_rwlock_upstreamsRoutedD);
#endif
    auto findData = upstreamsRoutedD.find(this->nowSimulationStep);
    if (findData != upstreamsRoutedD.end()) {
        upstreamsRoutedD.erase(this->nowSimulationStep);
    }
#ifdef USE_PTHREAD
    pthread_rwlock_unlock(&this->pthread_rwlock_upstreamsRoutedD);
#endif
}

SimulationSubbasin::~SimulationSubbasin() {
}

void SimulationSubbasin::handleHadOutputDatas() {
    if (nowSimulationStep == Context::getInstance()->getSimulationSteps() && isOutlet()) {
        printf("P%lu: outlet(NodeId=%lu)'s flow is %f on timestep=%lu\n",
               mpiutil::proid, nodeid,
               this->nowFolwData.getFlow(),
               nowSimulationStep);
    }
#ifdef USE_PTHREAD
    pthread_rwlock_destroy(&this->pthread_rwlock_upstreamsRoutedD);
#endif
}

void SimulationSubbasin::setModelContext(ModelContext *pModelContext) {
    this->pModelContext = pModelContext;
}

void SimulationSubbasin::setModels(BaseModel *pRunoffModel, BaseModel *pRoutingModel) {
    this->pRunoffModel = pRunoffModel;
    this->pRoutingModel = pRoutingModel;
}
