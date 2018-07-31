//
// Created by wujiahao on 18-5-10.
//

#include "routed_data_imessage.h"
#include "../simulation/trouting_data.h"
#include "../utils/mpiutil.h"

RoutedDataIMessage::RoutedDataIMessage() {}

void RoutedDataIMessage::updateTRoutingData(const TRoutingData &newData) {
    this->tRoutingData = newData;
}

void RoutedDataIMessage::msgIsend(utils::_type_proid destProid, const TRoutingData &newData) {
    this->updateTRoutingData(newData);
    this->requestTag = MPI_REQUEST_NULL;
    MPI_Isend(&this->tRoutingData, sizeof(TRoutingData), MPI_BYTE, destProid,
              mpiutil::MPI_ROUTING_MSG_TAG, MPI_COMM_WORLD, &this->requestTag);
//    MPI_Send_init(&this->tRoutingData, sizeof(TRoutingData), MPI_BYTE, destProid,
//                  mpiutil::MPI_ROUTING_MSG_TAG, MPI_COMM_WORLD, &this->requestTag);
//    MPI_Start(&this->requestTag);
    availableTag = false;
}

void RoutedDataIMessage::msgIrecv() {
    this->requestTag = MPI_REQUEST_NULL;
    MPI_Irecv(&this->tRoutingData, sizeof(TRoutingData), MPI_BYTE, MPI_ANY_SOURCE,
              mpiutil::MPI_ROUTING_MSG_TAG, MPI_COMM_WORLD, &this->requestTag);
    availableTag = false;
}
//
//void RoutedDataIMessage::msgIrecv(MPI_Message msg) {
//    this->requestTag = MPI_REQUEST_NULL;
//    MPI_Imrecv(&this->tRoutingData, sizeof(TRoutingData), MPI_BYTE, &msg, &this->requestTag);
////    MPI_Mrecv(&this->tRoutingData, sizeof(TRoutingData), MPI_BYTE, &msg, MPI_STATUS_IGNORE);
//    availableTag = false;
//}

bool RoutedDataIMessage::isAvailable() {
    return this->availableTag;
}

MPI_Request *RoutedDataIMessage::getRequestTag() {
    return &(this->requestTag);
}

const TRoutingData &RoutedDataIMessage::getTRoutingData() {
    return this->tRoutingData;
}

void RoutedDataIMessage::makeAvailable() {
    this->availableTag = true;
}




