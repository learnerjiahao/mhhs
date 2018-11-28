//
// Created by wujiahao on 18-6-29.
//

#ifndef MHHSS_XINANJIANG2_MODEL_H
#define MHHSS_XINANJIANG2_MODEL_H


#include "base_model.h"
#include <list>

class XinAnJiang2Model : public BaseModel{
public:
    XinAnJiang2Model(std::map<std::string, double> &paraDatas, std::map<std::string, double> &initDatas);
    XinAnJiang2Model();
    //基本参数
    static const std::string P_F_NAME;        // 流域面积（km2）
    static const std::string P_DT_NAME;  // 每一步长的小时数

    static const std::string P_K_NAME;          // 流域蒸散发能力与实测水面蒸发之比
    static const std::string P_IMP_NAME;      // 流域不透水面积占全流域面积之比
    static const std::string P_B_NAME;          // 蓄水容量曲线的方次
    static const std::string P_WUM_NAME;      // 上下深层流域蓄水容量（mm）
    static const std::string P_WLM_NAME;
    static const std::string P_WDM_NAME;
    static const std::string P_C_NAME;          // 深层蒸散发系数
    static const std::string P_FC_NAME;          // 稳定入渗率（毫米／小时）
    static const std::string P_KKG_NAME;      // 地下径流消退系数
    static const std::string P_KSTOR_NAME;  // day脉冲汇流计算的参数,Liang
//    ap.LowerBnd[0]=0.0001;	ap.UpperBnd[0]=1;
//    ap.LowerBnd[1]=0.0001;	ap.UpperBnd[1]=0.5;
//    ap.LowerBnd[2]=0.0001;	ap.UpperBnd[2]=1;
//    ap.LowerBnd[3]=5;		ap.UpperBnd[3]=30;
//    ap.LowerBnd[4]=50;		ap.UpperBnd[4]=100;
//    ap.LowerBnd[5]=50;		ap.UpperBnd[5]=200;
//    ap.LowerBnd[6]=0.0001;	ap.UpperBnd[6]=0.3;
//    ap.LowerBnd[7]=0.0001;	ap.UpperBnd[7]=50;
//    ap.LowerBnd[8]=0.0001;	ap.UpperBnd[8]=0.99;
//    ap.LowerBnd[9]=1;		ap.UpperBnd[9]=6;

    //状态
    static const std::string I_WU_NAME;        // 上下深层流域蓄水量（mm）
    static const std::string I_WL_NAME;
    static const std::string I_WD_NAME;
    static const std::string I_QRG0_NAME;    // 地下水初始流量（m3/s）

    // input data name
    static const std::string ID_P_NAME;
    static const std::string ID_EI_NAME;


private:
    std::list<double> v_RS_tmp;
    double stateNowRS, stateNowRG;

public:
    void loadModelParaDatas(std::string filepath) override;

    void loadModelInitDatas(std::string filepath) override;

    void loadModelInputDatas(std::string filepath) override;

protected:
    void produceFlowSimul(std::map<std::string, double> &inputDatas, int nowTimestep) override;

    RoutingDataMeta
    routeFlowSimul(std::map<std::string, double> &inputDatas, int nowTimestep, RoutingDataMeta &upRoutedDatas) override;
};


#endif //MHHSS_XINANJIANG2_MODEL_H
