//
// Created by wujiahao on 2018/11/24.
//

#ifndef PNOHS_ALPHA_HIMS_PRODUCER_H
#define PNOHS_ALPHA_HIMS_PRODUCER_H


#include "../base_model.h"
#include "../model_producer.h"

class HIMSModelProducer : public ModelProducer {
private:
    static HIMSModelProducer *instance;
    HIMSModelProducer() = default;

public:
    BaseModel *newModel(ModelContext *pModelContext) override;
    static HIMSModelProducer *getInstance();
};


#endif //PNOHS_ALPHA_HIMS_PRODUCER_H
