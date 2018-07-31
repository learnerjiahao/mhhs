//
// Created by wujiahao on 18-5-11.
//

#ifndef MHHSS_SUBBASINS_CONTAINER_H
#define MHHSS_SUBBASINS_CONTAINER_H

#include <list>
#include <mpi.h>
#include "../subbasin/simulation_subbasin.h"

class SubbasinsContainer {
public:

    static SubbasinsContainer *getInstance(utils::_type_time_step allSimulationSteps);
    ~SubbasinsContainer();

    std::map<utils::_type_nodeid, SimulationSubbasin> *subbasins;
    std::map<utils::_type_time_step, std::list<utils::_type_nodeid>> sortCt;   // used for pick which subbasin should be simulation first

    void addNewSubbasin(utils::_type_nodeid nodeid, const SimulationSubbasin &subbasin);

    bool checkOneStreamLocated(utils::_type_nodeid id);
    bool isAllFinished();

    bool tryAppendingRoutingData(TRoutingData &data, MPI_Request &request, int &recvFlag);
    void appendRoutingData(const TRoutingData &data);
    void appendRoutingDatas(std::vector<TRoutingData> &datas);

    void sortCt4PickingSimu(utils::_type_time_step timeStep);
    void initSortCt();

    SimulationSubbasin *pickAllSimulatable();
    SimulationSubbasin *pickAllSimulatableComplex();

    SimulationSubbasin *pickSlopeSimulatable();

    utils::_type_nodeid subbasinsSize();
    utils::_type_nodeid solveUpstreamsNeedComm();

    unsigned long getAllCommucationCount();

    void updateSortCt(utils::_type_time_step timestep, utils::_type_nodeid nodeid);

private:
    static SubbasinsContainer *sc_instance;
    utils::_type_time_step allSimulationSteps;
    SubbasinsContainer(utils::_type_time_step allSimulationSteps);
    SubbasinsContainer();

    static bool compare4SortPick(const utils::_type_nodeid &first, const utils::_type_nodeid &second);

};


#endif //MHHSS_SUBBASINS_CONTAINER_H
