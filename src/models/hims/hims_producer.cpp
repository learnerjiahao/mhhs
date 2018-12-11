//
// Created by wujiahao on 2018/11/24.
//

#include "hims_producer.h"
#include "hims_runoff_model.h"


HIMSModelProducer *HIMSModelProducer::instance = nullptr;

HIMSModelProducer *HIMSModelProducer::getInstance() {
    if (instance == nullptr)
        instance =  new HIMSModelProducer();
    return instance;
}

BaseModel *HIMSModelProducer::newModel(ModelContext *pModelContext) {
    return new HIMSRunoffModel(pModelContext);
}