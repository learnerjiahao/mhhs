//
// Created by wujiahao on 2018/11/22.
//

#ifndef MHHSS_BASE_MODEL_H
#define MHHSS_BASE_MODEL_H


#include <vector>
#include "../simulation/routing_data_meta.h"
#include "../utils/ret_msg.h"
#include "model_context.h"
#include "../readInputs/config.h"

class BaseModel {

protected:
    virtual RetMSG checkInitDatas(const std::map<std::string, double> &initDatas);
    virtual RetMSG checkParaDatas(const std::map<std::string, double> &paraDatas);
    virtual RetMSG checkInputDatas(const std::map<std::string, std::vector<double>> &inputDatas);

    RetMSG checkDatas(const std::map<std::string, double> &datas, const std::vector<std::string> &dataNames);

    RetMSG checkModelDatas(const ModelContext *pModelContext);

public:
    BaseModel(const ModelContext *pModelContext);
    virtual RoutingDataMeta runModel(const ModelContext &pModelContext,
                                     const Config &configValues,
                                     const RoutingDataMeta &upRoutDatas,
                                     int nowTimeStep) = 0;

    virtual std::vector<std::string> getParaNames() = 0;
    virtual std::vector<std::string> getInitNames() = 0;
    virtual std::vector<std::string> getInputNames() = 0;

};


#endif //MHHSS_BASE_MODEL_H
