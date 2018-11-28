//
// Created by parallels on 11/28/18.
//

#include "xaj3_runoff_model.h"

RoutingDataMeta XAJ3RunoffModel::runModel(const ModelContext &pModelContext, const ConfigValues &configValues,
                                          const RoutingDataMeta &upRoutDatas, int nowTimeStep) {
    return RoutingDataMeta();
}

std::vector<std::string> XAJ3RunoffModel::getParaNames() {
    return std::vector<std::string>();
}

std::vector<std::string> XAJ3RunoffModel::getInitNames() {
    return std::vector<std::string>();
}

std::vector<std::string> XAJ3RunoffModel::getInputNames() {
    return std::vector<std::string>();
}

XAJ3RunoffModel::XAJ3RunoffModel(const ModelContext *pModelContext) : BaseModel(pModelContext) {}
