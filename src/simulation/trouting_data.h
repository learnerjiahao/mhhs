//
// Created by wujiahao on 18-5-7.
//

#ifndef MHHSS_TROUTING_DATA_H
#define MHHSS_TROUTING_DATA_H


#include "../utils/predefine.h"
#include "routing_data_meta.h"

class TRoutingData : public RoutingDataMeta{

private:
    utils::_type_time_step time_step;
    utils::_type_nodeid downstreamId;

public:
    TRoutingData(utils::_type_nodeid downstreamId, utils::_type_time_step time_step);
    TRoutingData();

    utils::_type_time_step getTimeStep() const;
    utils::_type_nodeid getDownstreamId() const;

    void setTimeStep(utils::_type_time_step time_step);
    void setDownstreamId(utils::_type_nodeid downstreamId);
};


#endif //MHHSS_TROUTING_DATA_H
