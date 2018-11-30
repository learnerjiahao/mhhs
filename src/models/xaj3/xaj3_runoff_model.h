//
// Created by parallels on 11/28/18.
//

#ifndef MHHSS_XAJ3_RUNOFF_MODEL_H
#define MHHSS_XAJ3_RUNOFF_MODEL_H


#include "../base_model.h"
#include <list>

class XAJ3RunoffModel : public BaseModel {
private:
    //基本参数
    double &WUM;      // 上下深层流域蓄水容量（mm）
    double &WLM;
    double &WDM;
    double &K;          // 蒸发皿系数
    double &C;          // 深层蒸散发系数
    double &B;          // 蓄水容量曲线指数
    double &IMP;      // 不透水面积比重
    double &SM;        // 自由水蓄水库容量（mm）
    double &EX;        // 自由水蓄水容量曲线指数
    double &KG;        // 地下水从自由水库的出流系数
    double &KSS;      // 壤中流出流系数KSS
    double &KKG;      // 地下水退水系数KKG
    double &KKSS;    // 壤中流退水系数KKSS
    double &KSTOR;  // day脉冲汇流计算的参数,Liang

    //状态
    double &WU;        // 上下深层流域蓄水量（mm）
    double &WL;
    double &WD;
    double &FR;        // 产流面积比重
    double &S;          // 自由水蓄水量（mm）
    double &QRSS0;  // 壤中流初始流量（m3/s）
    double &QRG0;    // 地下水初始流量（m3/s）

    // input data
    std::vector<double> &Ps;
    std::vector<double> &EIs;


    std::list<double> v_RS_tmp;
    double stateNowRS, stateNowRSS, stateNowRG;
    double statePrevQRSS, statePrevQRG;

public:
    static const std::vector<std::string> paraNames, initNames, inputNames;
    RoutingDataMeta
    runModel(const ModelContext &pModelContext, const Config &configValues, const RoutingDataMeta &upRoutDatas,
             int nowTimeStep) override;

    XAJ3RunoffModel(ModelContext *pModelContext);

    std::vector<std::string> getParaNames() override;

    std::vector<std::string> getInitNames() override;

    std::vector<std::string> getInputNames() override;
};


#endif //MHHSS_XAJ3_RUNOFF_MODEL_H
