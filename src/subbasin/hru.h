//
// Created by wujiahao on 18-5-6.
//

#ifndef MHHSS_HRU_H
#define MHHSS_HRU_H


#include "../utils/predefine.h"
#include "../simulation/routing_data_meta.h"

class HRU {
private:
    utils::_type_hruid hruid;
public:
    HRU(utils::_type_hruid hruid);
    void HRUSimulation(RoutingDataMeta &HRUPLRoutingData);

};


#endif //MHHSS_HRU_H
