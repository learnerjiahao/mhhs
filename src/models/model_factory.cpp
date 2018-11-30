//
// Created by parallels on 10/20/18.
//

#include "model_factory.h"

std::map<std::string, ModelProducer *> ModelFactory::models_registry;

bool ModelFactory::addModelRegistry(const std::string &model_name, ModelProducer *modelProdecer) {
    if (models_registry.find(model_name) != models_registry.end())
        return false;
    models_registry[model_name] = modelProdecer;
    return true;
}

bool ModelFactory::removeModelRegistry(const std::string &model_name) {
    auto iter = models_registry.find(model_name);
    if (iter == models_registry.end())
        return false;
    delete(iter->second);
    models_registry.erase(iter);
    return true;
}

ModelFactory::~ModelFactory() {
    for (auto iter = models_registry.begin(); iter != models_registry.end(); iter++)
    {
        delete(iter->second);
    }
}

BaseModel *ModelFactory::prodeceRunoffModel(const ModelContext *pModelContext) {
    return prodeceNewModel(pModelContext, true);
}

BaseModel *ModelFactory::prodeceRoutingModel(const ModelContext *pModelContext) {
    return prodeceNewModel(pModelContext, false);
}

BaseModel *ModelFactory::prodeceNewModel(const ModelContext *pModelContext, bool genRunoffModel) {
    std::string model_name;
    if (genRunoffModel) {
        model_name = pModelContext->runoffModelName;
    } else {
        model_name = pModelContext->routingModelName;
    }
    if (models_registry.find(model_name) == models_registry.end())
        return nullptr;
    return models_registry[model_name]->newModel(pModelContext);
}
