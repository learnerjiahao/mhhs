//
// Created by wujiahao on 18-6-20.
//

#ifndef MHHSS_BASE_MODEL_H
#define MHHSS_BASE_MODEL_H


#include <map>
#include <vector>
#include "../simulation/routing_data_meta.h"

class BaseModel {

protected:
    std::map<std::string, double> paraDatas;
    std::vector<std::string> paraDataNames;

    std::map<std::string, double> initDatas;
    std::vector<std::string> initDataNames;

    std::vector<std::string> inputDataNames;
    std::map<utils::_type_time_step ,std::map<std::string, double>> inputDatas;

public:
    BaseModel();

public:
    virtual ~BaseModel();

public:
    void runProduceFlowSimul(std::map<std::string,double> &inputDatas, int nowTimeStep);
    RoutingDataMeta runRouteFlowSimul(std::map<std::string,double> &inputDatas, int nowTimeStep, RoutingDataMeta &upRoutDatas);

public:
    void updateParaDatas(std::map<std::string,double> &paraDatas);
    virtual void loadModelParaDatas(std::string filepath) = 0;
    virtual void loadModelInitDatas(std::string filepath) = 0;
    virtual void loadModelInputDatas(std::string filepath) = 0;



    std::map<std::string, double> &getOneSteptimeInputDatas(int nowTimeStep);

protected:
    bool checkParaDatas(std::map<std::string, double> &paraDatas);
    bool checkInitDatas(std::map<std::string, double> &initDatas);

    bool checkDatas(std::map<std::string, double> &datas, std::vector<std::string> &dataNames);
    bool checkInputDatas(std::map<std::string, double> &paraDatas);

    double getParaValue(std::string paraKey);
    double getInitValue(std::string initDataKey);
    void updateInitValue(std::string initDataKey, double value);

    virtual void produceFlowSimul(std::map<std::string, double> &inputDatas, int nowTimestep) = 0;
    virtual RoutingDataMeta routeFlowSimul(std::map<std::string, double> &inputDatas, int nowTimestep,
                                           RoutingDataMeta &upRoutedDatas) = 0;


    void updateMapDatas(std::map<std::string, double> &targetMap, std::map<std::string, double> &srcMap);
};


#endif //MHHSS_BASE_MODEL_H
