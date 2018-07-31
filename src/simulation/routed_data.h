//
// Created by wujiahao on 18-5-7.
//

#ifndef MHHSS_ROUTED_DATA_H
#define MHHSS_ROUTED_DATA_H


#include "routing_data_meta.h"

class RoutedData : public RoutingDataMeta{

private:
    utils::_type_nodeid all_routed_tag;   // todo

public:
    RoutedData(utils::_type_nodeid all_routed_tag);

    void routing(const RoutingDataMeta &routingData);
    bool isAllRouted();

};


#endif //MHHSS_ROUTED_DATA_H
