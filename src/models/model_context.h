//
// Created by wujiahao on 2018/11/22.
//

#ifndef MHHSS_MODEL_CONTEXT_H
#define MHHSS_MODEL_CONTEXT_H


#include <vector>
#include <sstream>
#include "../utils/predefine.h"
#include "../utils/ret_msg.h"
#include "../readInputs/config.h"

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

    std::string runoffModelName;
    std::string routingModelName;

protected:
    RetMSG readMapValues(const std::string &filePath, std::map<std::string, double> &dataMap, const Config &configValues);
    RetMSG readInputDatas(const std::string &filePath, const Config &configValues);
    RetMSG readProperties(const std::string &filePath, const Config &configValues);
    RetMSG readObserDatas(const std::string &filePath, const Config &configValues);

public:
    ModelContext(utils::_type_nodeid _nodeid);
    RetMSG initContext(const Config &configValues);
};

#endif //MHHSS_MODEL_CONTEXT_H
