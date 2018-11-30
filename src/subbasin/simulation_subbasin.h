//
// Created by wujiahao on 18-5-6.
//

#ifndef MHHSS_SIMULATION_SUBBASIN_H
#define MHHSS_SIMULATION_SUBBASIN_H


#include <map>
#include "../models/base_model.h"
#include "subbasin.h"
#include "../simulation/routed_data.h"
#include "../simulation/trouting_data.h"

// todo tip：同一计算节点上的汇流到同一子流域的多个子流域的routingData可以叠加后再一次性发送过去

class SimulationSubbasin : public Subbasin {

private:
    std::map<utils::_type_time_step, RoutedData> upstreamsRoutedD;

#ifdef USE_PTHREAD
    pthread_rwlock_t pthread_rwlock_upstreamsRoutedD;
#endif

    utils::_type_time_step nowSimulationStep;
    bool isSlopeSimuedThisStep;

    std::map<utils::_type_nodeid, RoutingDataMeta> routingData2Downstream;
    RoutingDataMeta nowFolwData;

    ModelContext *pModelContext = nullptr;
    BaseModel *pRunoffModel = nullptr;
    BaseModel *pRoutingModel = nullptr;

private:

    void routingInSimulation();
    void routingOutSimulation();
    void afterOneStepSimu();

private:
    void handleHadOutputDatas();
    void deleteRoutedFinishedData();

public:
    SimulationSubbasin();
    ~SimulationSubbasin();

    void slopePLSimulation(); // PL : product flow
    const std::map<utils::_type_nodeid, RoutingDataMeta> &runOneSimulation();

    void appendRoutingData(const TRoutingData &tRoutingData);
    bool isCompleted(utils::_type_time_step allSimulationSteps);


    bool isWaiting4Simulation(utils::_type_time_step allSimulationSteps);
    bool isSlopeSimuedNowStep();

    utils::_type_time_step getNowSimuStep();

    void setModelContext(ModelContext *pModelContext);
    void setModels(BaseModel *pRunoffModel, BaseModel *pRoutingModel);

};


#endif //MHHSS_SIMULATION_SUBBASIN_H
