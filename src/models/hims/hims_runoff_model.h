//
// Created by wujiahao on 2018/11/24.
//

#ifndef PNOHS_ALPHA_HIMS_RUNOFF_MODEL_H
#define PNOHS_ALPHA_HIMS_RUNOFF_MODEL_H

#include "../base_model.h"

class HIMSRunoffModel : public BaseModel {
public:

    // 土壤水容量SMSC（mm）
    double &SMSC;
    // 实际蒸散发系数ε
    double &e;
    // 子流域产流参数R
    double &R;
    // 子流域产流参数r
    double &r;
    // 壤中流出流系数La
    double &La;
    // 地下水补给系数KG
    double &KG;
    // 子流域实际蒸发折算系数KC
    double &KC;
    // 子流域基流系数KB
    double &KB;
    // 地面径流日退水系数KI
    double &KI;
    // 地下径流日退水系数KKG
    double &KKG;
    // 壤中流日退水系数KKSS
    double &KKSS;

    // init state
    double &SWC0; // 非饱和土壤含水量
    double &GWs0; // 地下水蓄水量
    double &QRS0, &QRSS0, &QRG0; // 地表径流流量, 壤中流流量, 地下径流流量

    // now state
    double SWC; // 非饱和土壤含水量
    double GWs; // 地下水蓄水量
    double QRS, QRSS, QRG; // 地表径流流量, 壤中流流量, 地下径流流量

    std::vector<double> &Ps;
    std::vector<double> &maxTs;
    std::vector<double> &minTs;

public:
    RoutingDataMeta
    runModel(ModelContext &pModelContext, const Config &configValues, const RoutingDataMeta &upRoutDatas,
             int nowTimeStep) override;

    HIMSRunoffModel(ModelContext *pModelContext);

    std::vector<std::string> getParaNames(const ModelContext *pModelContext) override;

    std::vector<std::string> getInitNames(const ModelContext *pModelContext) override;

    std::vector<std::string> getInputNames(const ModelContext *pModelContext) override;
};


#endif //PNOHS_ALPHA_HIMS_RUNOFF_MODEL_H
