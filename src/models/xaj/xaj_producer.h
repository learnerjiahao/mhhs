//
// Created by parallels on 10/20/18.
//

#ifndef PNOHS_ALPHA_XAJ_PRODUCER_H
#define PNOHS_ALPHA_XAJ_PRODUCER_H


#include "xaj_runoff_model.h"
#include "../model_factory.h"

class XAJModelProducer : public ModelProducer
{
private:
    static XAJModelProducer *instance;
    XAJModelProducer() = default;

public:
    BaseModel *newModel(ModelContext *pModelContext) override;
    static XAJModelProducer *getInstance();
};
#endif //PNOHS_ALPHA_XAJ_PRODUCER_H
