//
// Created by wujiahao on 18-6-20.
//

#include "base_model.h"

void BaseModel::updateParaDatas(std::map<std::string, double> &paraDatas) {
    if (!checkParaDatas(paraDatas)) {
        // todo
        exit(-1);
    }
    updateMapDatas(this->paraDatas, paraDatas);
}

bool BaseModel::checkParaDatas(std::map<std::string, double> &paraDatas) {
    return checkDatas(paraDatas, this->paraDataNames);
}

bool BaseModel::checkInitDatas(std::map<std::string, double> &initDatas) {
    return checkDatas(initDatas, this->initDataNames);
}

bool BaseModel::checkInputDatas(std::map<std::string, double> &inputDatas) {
    return checkDatas(inputDatas, this->inputDataNames);
}

bool BaseModel::checkDatas(std::map<std::string, double> &datas, std::vector<std::string> &dataNames) {
    for (int i = 0; i < dataNames.size(); ++i) {
        if (datas.find(dataNames.at(i)) == datas.end()) {
            return false;
        }
    }
    return true;
}

void BaseModel::runProduceFlowSimul(std::map<std::string, double> &inputDatas, int nowTimeStep) {
    if (!checkInputDatas(inputDatas)) {
        // todo
        exit(-1);
    }
    produceFlowSimul(inputDatas, nowTimeStep);
}

RoutingDataMeta BaseModel::runRouteFlowSimul(std::map<std::string, double> &inputDatas, int nowTimeStep, RoutingDataMeta &upRoutDatas) {
    if (!checkInputDatas(inputDatas)) {
        // todo
        exit(-1);
    }
    return routeFlowSimul(inputDatas, nowTimeStep, upRoutDatas);
}

double BaseModel::getParaValue(std::string paraKey) {
    return this->paraDatas.at(paraKey);
}

double BaseModel::getInitValue(std::string initDataKey) {
    return this->initDatas.at(initDataKey);
}

void BaseModel::updateInitValue(std::string initDataKey, double value) {
    this->initDatas[initDataKey] = value;
}

void BaseModel::updateMapDatas(std::map<std::string, double> &targetMap, std::map<std::string, double> &srcMap) {
    for (std::map<std::string, double>::iterator it = srcMap.begin(); it != srcMap.end() ; ++it) {
        targetMap[it->first] = it->second;
    }
}

std::map<std::string, double> &BaseModel::getOneSteptimeInputDatas(int nowTimeStep) {
    return this->inputDatas.at(nowTimeStep);
}

BaseModel::~BaseModel() {

}

BaseModel::BaseModel() {}


