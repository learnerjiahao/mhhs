//
// Created by parallels on 10/21/18.
//

#include "xaj3_producer.h"

XAJ3ModelProducer *XAJ3ModelProducer::instance = new XAJ3ModelProducer();

XAJ3ModelProducer *XAJ3ModelProducer::getInstance() {
    return instance;
}

BaseModel *XAJ3ModelProducer::newModel(const ModelContext *pModelContext) {
    return new XAJ3RunoffModel(pModelContext);
}
