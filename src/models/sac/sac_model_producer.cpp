//
// Created by parallels on 10/21/18.
//

#include "sac_model_producer.h"
#include "sac_runoff_model.h"

SACModelProducer *SACModelProducer::instance = nullptr;

BaseModel *SACModelProducer::newModel(ModelContext *pModelContext) {
    return new SACRunoffModel(pModelContext);
}

SACModelProducer *SACModelProducer::getInstance() {
    if (instance == nullptr)
        instance =  new SACModelProducer();
    return instance;
}
