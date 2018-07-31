//
// Created by wujiahao on 18-5-10.
//

#ifndef MHHSS_ROUTED_DATA_IMESSAGE_H
#define MHHSS_ROUTED_DATA_IMESSAGE_H


#include <mpi.h>
#include "../simulation/trouting_data.h"

class RoutedDataIMessage {
private:
    TRoutingData tRoutingData;
    MPI_Request requestTag = MPI_REQUEST_NULL;
    bool availableTag = true;
    void updateTRoutingData(const TRoutingData& newData);

public:

    RoutedDataIMessage();
    void msgIsend(utils::_type_proid destProid, const TRoutingData &newData);
    void msgIrecv();
//    void msgIrecv(MPI_Message msg);

    bool isAvailable();
    MPI_Request *getRequestTag();
    const TRoutingData& getTRoutingData();
    void makeAvailable();

};


#endif //MHHSS_ROUTED_DATA_IMESSAGE_H
