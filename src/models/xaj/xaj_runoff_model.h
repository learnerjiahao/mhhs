//
// Created by wujiahao on 18-7-28.
//

#ifndef PNOHS_XAJ_RUNOFF_MODEL_H
#define PNOHS_XAJ_RUNOFF_MODEL_H

#include "../base_model.h"

/**
 * 新安江三水源模型（马斯京根）
 */
class XAJRunoffModel : public BaseModel {
public:

    static const std::string &MODEL_NAME;

    std::vector<std::string> getParaNames(const ModelContext *pModelContext) override;

    std::vector<std::string> getInitNames(const ModelContext *pModelContext) override;

    std::vector<std::string> getInputNames(const ModelContext *pModelContext) override;

    // initial value must be provided (generally, initial value is also state value)
//    double initSR;              // 初始除地面径流后自由蓄水量 todo not used???
    double initWU, initWL, initWD;    //初始上下深层流域蓄水量（mm）
    double initFR;              //初始产流面积比重　todo not used???
    double initS;               //初始自由水蓄水量（mm）
    double initQRSS;           //初始壤中流流量（m3/s）
    double initQRG;            //初始地下水流量（m3/s）
    double initQRS;            //初始地表流量（m3/s）

    //state value
    double nowSR;              // 除地面径流后自由蓄水量
    double nowWU, nowWL, nowWD;      //上下深层流域蓄水量（mm）
    double nowFR;              //产流面积比重
    double nowS;               //自由水蓄水量（mm）
    double nowQRSS;           //壤中流流量（m3/s）
    double nowQRG;            //地下水流量（m3/s）
    double nowQRS;            //地表流量（m3/s）

private:
    double &WUM;  // 流域平均上层蓄水容量WUM（mm）
    double &WLM;  // 流域平均下层蓄水容量WLM（mm）
    double &WDM; // 流域平均深层蓄水容量WDM（mm）

    double &K;  // 流域蒸发折算系数	0.72	0~1
    double &C;   // 深层蒸散发折算系数	0.15	0.1~0.2
    double &B;   // 张力水蓄水容量曲线指数	0.2	0.1~0.4
    double &IMP;  // 不透水面积占全流域面积的比例	0.01	0.01~0.02
    double &SM;  // 表层自由水蓄水容量（mm）	40	10~50
    double &EX;  // 表层自由水蓄水容量曲线指数	1.2	1.0~1.5
    double &KG;  // 表层自由水蓄水库对地下水的日出流系数	0.35	KG+KI<1
    double &KSS;  // 表层自由水蓄水库对壤中流的日出流系数	0.35

    double &KI;  // 地面径流消退系数	0.2	0~1
    double &KKG;  // 地下水消退系数	0.94	0~1
    double &KKSS;  // 壤中流消退系数	0.6	0~1

    // input data
    std::vector<double> &Ps;
    std::vector<double> &EIs;
public:
    RoutingDataMeta
    runModel(ModelContext &pModelContext, const Config &configValues, const RoutingDataMeta &upRoutDatas,
             int nowTimeStep) override;

    XAJRunoffModel(ModelContext *pModelContext);
};


#endif //PNOHS_XAJ_RUNOFF_MODEL_H
