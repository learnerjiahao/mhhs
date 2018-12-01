//
// Created by wujiahao on 18-5-6.
//

#include "routing_data_meta.h"

void RoutingDataMeta::addMetaData(const RoutingDataMeta &secondData) {
    this->flow =  this->flow + secondData.getFlow();
}

RoutingDataMeta::RoutingDataMeta() : flow(0.0) {}

void RoutingDataMeta::setFlow(double flow) {
    this->flow = flow;
}

double RoutingDataMeta::getFlow() const {
    return this->flow;
}

void RoutingDataMeta::updateMetaData(const RoutingDataMeta &secondData) {
    this->flow = secondData.getFlow();
}

RoutingDataMeta::RoutingDataMeta(float flow) : flow(flow) {}
