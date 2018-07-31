//
// Created by wujiahao on 18-5-7.
//

#include "trouting_data.h"
#include "routed_data.h"

utils::_type_time_step TRoutingData::getTimeStep() const {
    return this->time_step;
}

utils::_type_nodeid TRoutingData::getDownstreamId() const {
    return downstreamId;
}

void TRoutingData::setTimeStep(utils::_type_time_step time_step) {
    this->time_step = time_step;
}

void TRoutingData::setDownstreamId(utils::_type_nodeid downstreamId){
    this->downstreamId = downstreamId;
}

TRoutingData::TRoutingData(utils::_type_nodeid downstreamId, utils::_type_time_step time_step)
        : RoutingDataMeta(), downstreamId(downstreamId), time_step(time_step) {}

TRoutingData::TRoutingData() : RoutingDataMeta(), time_step(0), downstreamId(0) {}