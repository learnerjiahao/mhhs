//
// Created by parallels on 10/21/18.
//

#ifndef PNOHS_ALPHA_MSKG_MODEL_FACTORY_H
#define PNOHS_ALPHA_MSKG_MODEL_FACTORY_H


#include "../model_producer.h"

class MSKGModelProducer : public ModelProducer {
private:
    static MSKGModelProducer *instance;
    MSKGModelProducer() = default;

public:
    BaseModel *newModel(ModelContext *pModelContext) override;
    static MSKGModelProducer *getInstance();
};


#endif //PNOHS_ALPHA_MSKG_MODEL_FACTORY_H
