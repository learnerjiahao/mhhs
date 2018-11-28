//
// Created by wujiahao on 18-6-19.
//

#ifndef MHHSS_XINANJIANGMODEL_H
#define MHHSS_XINANJIANGMODEL_H

#include <cmath>
#include <list>
#include "base_model.h"

/**
%% 三水源新安江模型
%
% 三水源——地表径流、壤中流、地下径流新安江模型计算流量过程
%
% 输入
%       rainfall，降水量文件路径
%           每行一条记录：时间，降水量(mm)
%       epan，蒸发皿蒸发量文件路径
%           每行一条记录：时间，蒸发皿蒸发量(mm)
%       parameter，基本参数文件路径
%           每行一个参数：
%           计算时间步长（hour）
%           计算总时长（hour）
%           流域面积F（km2）
%           上层流域蓄水容量WUM（mm）
%           下层流域蓄水容量WLM（mm）
%           深层流域蓄水容量WDM（mm）
%			蒸发皿系数K
%			深层蒸散发系数C
%			蓄水容量曲线指数B
%			不透水面积比重IMP
%			自由水蓄水库容量SM（mm）
%			自由水蓄水容量曲线指数EX
%			地下水从自由水库的出流系数KG
%			壤中流出流系数KSS
%			地下水退水系数KKG
%			壤中流退水系数KKSS
%           计算时段DT
%			无因次单位线时段UH
%			上层流域蓄水量WU（mm）
%			下层流域蓄水量WL（mm）
%			深层流域蓄水量WD（mm）
%			产流面积比重FR
%			自由水蓄水量S（mm）
%			壤中流初始流量QRSS0（m3/s）
%			地下水初始流量QRG0（m3/s）
%
% 输出
%       QR，总径流，m3/s
%       QRS，地表径流，m3/s
%       QRSS，壤中流，m3/s
%       QRG，地下径流，m3/s
 */

class XinAnJiang3Model : public BaseModel {

public:
    //基本参数
    static const std::string P_F_NAME;        // 流域面积（km2）

    static const std::string P_WUM_NAME;      // 上下深层流域蓄水容量（mm）
    static const std::string P_WLM_NAME;
    static const std::string P_WDM_NAME;
    static const std::string P_K_NAME;          // 蒸发皿系数
    static const std::string P_C_NAME;          // 深层蒸散发系数
    static const std::string P_B_NAME;          // 蓄水容量曲线指数
    static const std::string P_IMP_NAME;      // 不透水面积比重
    static const std::string P_SM_NAME;        // 自由水蓄水库容量（mm）
    static const std::string P_EX_NAME;        // 自由水蓄水容量曲线指数
    static const std::string P_KG_NAME;        // 地下水从自由水库的出流系数
    static const std::string P_KSS_NAME;      // 壤中流出流系数KSS
    static const std::string P_KKG_NAME;      // 地下水退水系数KKG
    static const std::string P_KKSS_NAME;    // 壤中流退水系数KKSS
    static const std::string P_DT_NAME;        // 计算时段DT
    static const std::string P_KSTOR_NAME;  // day脉冲汇流计算的参数,Liang

    //状态
    static const std::string I_WU_NAME;        // 上下深层流域蓄水量（mm）
    static const std::string I_WL_NAME;
    static const std::string I_WD_NAME;
    static const std::string I_FR_NAME;        // 产流面积比重
    static const std::string I_S_NAME;          // 自由水蓄水量（mm）
    static const std::string I_QRSS0_NAME;  // 壤中流初始流量（m3/s）
    static const std::string I_QRG0_NAME;    // 地下水初始流量（m3/s）

    // input data name
    static const std::string ID_P_NAME;
    static const std::string ID_EI_NAME;

private:
    std::list<double> v_RS_tmp;
    double stateNowRS, stateNowRSS, stateNowRG;
    double statePrevQRSS, statePrevQRG;

public:
    XinAnJiang3Model(std::map<std::string, double> &paraDatas, std::map<std::string, double> &initDatas);
    XinAnJiang3Model();
    ~XinAnJiang3Model();

    static void createModelParaDatas(std::map<std::string, double> &paraDatas,
                                     double F, double WUM, double WLM,
                                     double WDM, double K, double C,
                                     double B, double IMP, double SM,
                                     double EX, double KG, double KSS,
                                     double KKG, double KKSS, int DT, double KSTOR);

    static void createModelInitDatas(std::map<std::string, double> &initDatas,
                                     double WU, double WL, double WD,
                                     double FR, double S, double QRSS0, double QRG0);

    static void createModelInputDatas(std::map<std::string, double> &inputDatas,
                                     double P, double EI);

    void loadModelParaDatas(std::string filepath) override;

    void loadModelInitDatas(std::string filepath) override;

    void loadModelInputDatas(std::string filepath) override;

protected:

    void produceFlowSimul(std::map<std::string, double> &inputDatas, int nowTimestep) override;

    RoutingDataMeta routeFlowSimul(std::map<std::string, double> &inputDatas, int nowTimestep,
                                   RoutingDataMeta &upRoutedDatas) override;

};


#endif //MHHSS_XINANJIANGMODEL_H
