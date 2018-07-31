//
// Created by wujiahao on 18-5-10.
//

#ifndef MHHSS_ROUTED_DATA_IMSGS_POOL_H
#define MHHSS_ROUTED_DATA_IMSGS_POOL_H


#include <vector>
#include "routed_data_imessage.h"

class RoutedDataIMsgsPool {

private:
    std::vector<RoutedDataIMessage *> *msgsPool;
    utils::_type_nodeid initCap;

public:
    RoutedDataIMsgsPool(utils::_type_nodeid initCap);
    ~RoutedDataIMsgsPool();

    void chooseAvailableAndIsendNoWait(utils::_type_proid destProid, const TRoutingData &data);
    void chooseAvailableAndIsendMayWait(utils::_type_proid destProid, const TRoutingData &data);

    void chooseAllAvail2IrecvAndSaveFinNoWait(std::vector<TRoutingData> &hadRecvedDatas);
    void chooseAllAvail2IrecvAndSaveFinNoWaitButDy(std::vector<TRoutingData> &hadRecvedDatas);
    void chooseAllAvail2IrecvAndSaveFinMayWait(std::vector<TRoutingData> &hadRecvedDatas);

    // todo control the size of msgsPool delete redundant and available RoutedDataIMessage int the msgsPool
};


#endif //MHHSS_ROUTED_DATA_IMSGS_POOL_H
