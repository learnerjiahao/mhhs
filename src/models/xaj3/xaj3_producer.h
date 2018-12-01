//
// Created by parallels on 10/20/18.
//

#ifndef PNOHS_ALPHA_XAJ3_PRODUCER_H
#define PNOHS_ALPHA_XAJ3_PRODUCER_H


#include "xaj3_runoff_model.h"
#include "../model_factory.h"

class XAJ3ModelProducer: public ModelProducer
{
private:
    static XAJ3ModelProducer *instance;
    XAJ3ModelProducer() = default;

public:
    BaseModel *newModel(ModelContext *pModelContext) override;
    static XAJ3ModelProducer *getInstance();
};
#endif //PNOHS_ALPHA_XAJ3_PRODUCER_H
