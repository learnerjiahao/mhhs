//
// Created by wujiahao on 2018/11/22.
//

#ifndef MHHSS_MODEL_CONTEXT_H
#define MHHSS_MODEL_CONTEXT_H


#include <vector>
#include <sstream>
#include "base/values_container.h"
#include "../utils/predefine.h"
#include "../utils/ret_msg.h"
#include "../config_values.h"

class ModelContext {

public:
    utils::_type_nodeid &nodeid;
    std::map<std::string, double> params;
    std::map<std::string, double> initDatas;
    std::map<std::string, std::vector<double>> inputDatas;
    std::vector<double> obserDatas;

    double area;
    double length;
    double latitude, longitude, elev;

    bool hadObser;

    std::string runoffModel;
    std::string routingModel;

protected:
    RetMSG readMapValues(const std::string &filePath, std::map<std::string, double> &dataMap, const ConfigValues &configValues);
    RetMSG readInputDatas(const std::string &filePath, const ConfigValues &configValues);
    RetMSG readProperties(const std::string &filePath, const ConfigValues &configValues);
    RetMSG readObserDatas(const std::string &filePath, const ConfigValues &configValues);

public:
    ModelContext(utils::_type_nodeid _nodeid);
    RetMSG initContext(const ConfigValues &configValues);
};

#endif //MHHSS_MODEL_CONTEXT_H
