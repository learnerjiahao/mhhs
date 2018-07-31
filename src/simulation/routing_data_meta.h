//
// Created by wujiahao on 18-5-6.
//

#ifndef MHHSS_ROUTING_DATA_H
#define MHHSS_ROUTING_DATA_H


#include "../utils/predefine.h"

class RoutingDataMeta {

private:
    double flow;

public:
    RoutingDataMeta();
    void addMetaData(const RoutingDataMeta &secondData);
    void updateMetaData(const RoutingDataMeta &secondData);

    double getFlow() const;
    void setFlow(double flow);
};


#endif //MHHSS_ROUTING_DATA_H
