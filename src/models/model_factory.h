//
// Created by parallels on 10/20/18.
//

#ifndef PNOHS_ALPHA_MODEL_FACTORY_H
#define PNOHS_ALPHA_MODEL_FACTORY_H

#include <map>
#include "model_producer.h"

class ModelFactory {

protected:
    static std::map<std::string, ModelProducer *> models_registry;

public:

    static BaseModel *prodeceNewModel(const ModelContext *pModelContext, const std::string &model_name);

    static bool addModelRegistry(const std::string &model_name, ModelProducer *modelProdecer);

    static bool removeModelRegistry(const std::string &model_name);

    ~ModelFactory();

};



#endif //PNOHS_ALPHA_MODEL_FACTORY_H
