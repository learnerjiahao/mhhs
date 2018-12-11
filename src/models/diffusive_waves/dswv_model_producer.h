//
// Created by parallels on 10/21/18.
//

#ifndef PNOHS_ALPHA_DSWV_MODEL_PRODUCER_H
#define PNOHS_ALPHA_DSWV_MODEL_PRODUCER_H


#include "../model_producer.h"

class DSWVModelProducer : public ModelProducer {
private:
    static DSWVModelProducer *instance;
    DSWVModelProducer() = default;

public:
    BaseModel *newModel(ModelContext *pModelContext) override;
    static DSWVModelProducer *getInstance();
};


#endif //PNOHS_ALPHA_DSWV_MODEL_PRODUCER_H
