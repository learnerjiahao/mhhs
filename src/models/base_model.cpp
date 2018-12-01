//
// Created by wujiahao on 2018/11/22.
//

#include "base_model.h"
#include "../utils/mpiutil.h"

RetMSG BaseModel::checkInitDatas(const ModelContext *pModelContext) {
    RetMSG msg = checkDatas(pModelContext->initDatas, this->initNames);
    if (!msg.isSuccess()) {
        return RetMSG("init data: " + msg.getMsg(), -1);
    }
    return RetMSG();
}

RetMSG BaseModel::checkParaDatas(const ModelContext *pModelContext) {
    RetMSG msg = checkDatas(pModelContext->params, this->paraNames);
    if (!msg.isSuccess()) {
        return RetMSG("param data: " + msg.getMsg(), -1);
    }
    return RetMSG();
}

RetMSG BaseModel::checkInputDatas(const ModelContext *pModelContext) {
    std::vector<std::string> inputNames = this->inputNames;
    for (int i = 0; i < inputNames.size(); ++i) {
        if (pModelContext->inputDatas.find(inputNames.at(i)) == pModelContext->inputDatas.end()) {
            return RetMSG("input data:" + inputNames.at(i) + " can not be found", -1);
        }
    }
    return RetMSG();
}

RetMSG BaseModel::checkDatas(const std::map<std::string, double> &datas, const std::vector<std::string> &dataNames) {
    for (int i = 0; i < dataNames.size(); ++i) {
        if (datas.find(dataNames.at(i)) == datas.end()) {
            return RetMSG(dataNames.at(i) + " can not be found", -1);
        }
    }
    return RetMSG();
}

RetMSG BaseModel::checkModelDatas(const ModelContext *pModelContext) {
    RetMSG msg = checkInitDatas(pModelContext);
    if (!msg.isSuccess()) {
        return RetMSG("subbasin(" + std::to_string(pModelContext->nodeid) + "): " + msg.getMsg(), -1);
    }
    msg = checkParaDatas(pModelContext);
    if (!msg.isSuccess()) {
        return RetMSG("subbasin(" + std::to_string(pModelContext->nodeid) + "): " + msg.getMsg(), -1);
    }
    msg = checkInputDatas(pModelContext);
    if (!msg.isSuccess()) {
        return RetMSG("subbasin(" + std::to_string(pModelContext->nodeid) + "): " + msg.getMsg(), -1);
    }
    return RetMSG();
}

BaseModel::BaseModel(const ModelContext *pModelContext) {}

std::vector<std::string> BaseModel::getParaNames(const ModelContext *pModelContext) {
    return this->paraNames;
}

std::vector<std::string> BaseModel::getInitNames(const ModelContext *pModelContext) {
    return this->initNames;
}

std::vector<std::string> BaseModel::getInputNames(const ModelContext *pModelContext) {
    return this->inputNames;
}
