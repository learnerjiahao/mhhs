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
    virtual RetMSG checkInitDatas(const ModelContext *pModelContext);
    virtual RetMSG checkParaDatas(const ModelContext *pModelContext);
    virtual RetMSG checkInputDatas(const ModelContext *pModelContext);

    RetMSG checkDatas(const std::map<std::string, double> &datas, const std::vector<std::string> &dataNames);

//    std::vector<std::string> paraNames, initNames, inputNames;

public:
    BaseModel(const ModelContext *pModelContext);
    virtual RoutingDataMeta runModel(ModelContext &pModelContext,
                                     const Config &configValues,
                                     const RoutingDataMeta &upRoutDatas,
                                     int nowTimeStep) = 0;

    RetMSG checkModelDatas(const ModelContext *pModelContext);

    virtual std::vector<std::string> getParaNames(const ModelContext *pModelContext) = 0;
    virtual std::vector<std::string> getInitNames(const ModelContext *pModelContext) = 0;
    virtual std::vector<std::string> getInputNames(const ModelContext *pModelContext) = 0;

};


#endif //MHHSS_BASE_MODEL_H
