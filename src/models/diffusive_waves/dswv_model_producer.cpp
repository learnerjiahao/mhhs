//
// Created by parallels on 10/21/18.
//

#include "dswv_model_producer.h"
#include "diffusive_waves_routing_model.h"

DSWVModelProducer *DSWVModelProducer::instance = nullptr;

BaseModel *DSWVModelProducer::newModel(ModelContext *pModelContext) {
    return new DiffusiveWavesRoutingModel(pModelContext);
}

DSWVModelProducer *DSWVModelProducer::getInstance() {
    if (instance == nullptr)
        instance =  new DSWVModelProducer();
    return instance;
}