//
// Created by wujiahao on 18-5-7.
//

#include "routed_data.h"

void RoutedData::routing(const RoutingDataMeta &routingData) {
    this->addMetaData(routingData);
    this->all_routed_tag --;
}

bool RoutedData::isAllRouted() {
    return (this->all_routed_tag <= 0);
}

RoutedData::RoutedData(utils::_type_nodeid all_routed_tag)
        : RoutingDataMeta(), all_routed_tag(all_routed_tag) {}
