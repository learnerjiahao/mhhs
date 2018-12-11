//
// Created by parallels on 10/21/18.
//

#include "xaj_producer.h"
#include "xaj_runoff_model.h"

XAJModelProducer *XAJModelProducer::instance = nullptr;

XAJModelProducer *XAJModelProducer::getInstance() {
    if (instance == nullptr)
        instance =  new XAJModelProducer();
    return instance;
}

BaseModel *XAJModelProducer::newModel(ModelContext *pModelContext) {
    return new XAJRunoffModel(pModelContext);
}
