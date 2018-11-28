//
// Created by parallels on 10/20/18.
//

#ifndef PNOHS_ALPHA_ROUTINGMODELPRODECER_H
#define PNOHS_ALPHA_ROUTINGMODELPRODECER_H


#include "base_model.h"

class ModelProducer {
public:
    virtual BaseModel *newModel(const ModelContext *pModelContext) = 0;
};


#endif //PNOHS_ALPHA_ROUTINGMODELPRODECER_H
