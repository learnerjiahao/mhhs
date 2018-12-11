//
// Created by parallels on 10/21/18.
//

#ifndef PNOHS_ALPHA_SAC_MODEL_PRODUCER_H
#define PNOHS_ALPHA_SAC_MODEL_PRODUCER_H


#include "../base_model.h"
#include "../model_producer.h"

class SACModelProducer : public ModelProducer {
private:
    static SACModelProducer *instance;
    SACModelProducer() = default;

public:
    BaseModel *newModel(ModelContext *pModelContext) override;
    static SACModelProducer *getInstance();
};


#endif //PNOHS_ALPHA_SAC_MODEL_PRODUCER_H
