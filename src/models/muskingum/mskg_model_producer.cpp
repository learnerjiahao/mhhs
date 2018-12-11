//
// Created by parallels on 10/21/18.
//

#include "mskg_model_producer.h"
#include "muskingum_routing_model.h"

MSKGModelProducer *MSKGModelProducer::instance = nullptr;

BaseModel *MSKGModelProducer::newModel(ModelContext *pModelCOntext) {
    return new MuskingumRoutingModel(pModelCOntext);
}

MSKGModelProducer *MSKGModelProducer::getInstance() {
    if (instance == nullptr)
        instance =  new MSKGModelProducer();
    return instance;
}
