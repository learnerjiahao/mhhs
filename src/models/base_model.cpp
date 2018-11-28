//
// Created by wujiahao on 2018/11/22.
//

#include "base_model.h"
#include "../utils/mpiutil.h"

RetMSG BaseModel::checkInitDatas(const std::map<std::string, double> &initDatas) {
    RetMSG msg = checkDatas(initDatas, this->getInitNames());
    if (!msg.isSuccess()) {
        return RetMSG("init data: " + msg.getMsg(), -1);
    }
    return RetMSG();
}

RetMSG BaseModel::checkParaDatas(const std::map<std::string, double> &paraDatas) {
    RetMSG msg = checkDatas(paraDatas, this->getParaNames());
    if (!msg.isSuccess()) {
        return RetMSG("param data: " + msg.getMsg(), -1);
    }
    return RetMSG();
}

RetMSG BaseModel::checkInputDatas(const std::map<std::string, std::vector<double>> &inputDatas) {
    std::vector<std::string> inputNames = this->getInputNames();
    for (int i = 0; i < inputNames.size(); ++i) {
        if (inputDatas.find(inputNames.at(i)) == inputDatas.end()) {
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

RetMSG BaseModel::checkModelDatas(const ModelContext *pModelContext,) {
    RetMSG msg = checkInitDatas(pModelContext->initDatas);
    if (!msg.isSuccess()) {
        return RetMSG("subbasin(" + std::to_string(pModelContext->nodeid) + "): " + msg.getMsg(), -1);
    }
    msg = checkParaDatas(pModelContext->params);
    if (!msg.isSuccess()) {
        return RetMSG("subbasin(" + std::to_string(pModelContext->nodeid) + "): " + msg.getMsg(), -1);
    }
    msg = checkInputDatas(pModelContext->inputDatas);
    if (!msg.isSuccess()) {
        return RetMSG("subbasin(" + std::to_string(pModelContext->nodeid) + "): " + msg.getMsg(), -1);
    }
    return RetMSG();
}

BaseModel::BaseModel(const ModelContext *pModelContext) {
    RetMSG msg = checkModelDatas(pModelContext);
    if (!msg.isSuccess()) {
        mpiutil::mpiAbort(msg.getMsg(), msg.getErrCode(), false);
    }
}
