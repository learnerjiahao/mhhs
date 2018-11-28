//
// Created by wujiahao on 18-6-29.
//

#include <cmath>
#include <fstream>
#include "xinanjiang2_model.h"

//基本参数
const std::string XinAnJiang2Model::P_K_NAME = "K";          // 蒸发皿系数
const std::string XinAnJiang2Model::P_IMP_NAME = "IMP";      // 不透水面积比重
const std::string XinAnJiang2Model::P_B_NAME = "B";
const std::string XinAnJiang2Model::P_WUM_NAME = "WUM";      // 上下深层流域蓄水容量（mm）
const std::string XinAnJiang2Model::P_WLM_NAME = "WLM";
const std::string XinAnJiang2Model::P_WDM_NAME = "WDM";         // 蓄水容量曲线指数
const std::string XinAnJiang2Model::P_C_NAME = "C";          // 深层蒸散发系数
const std::string XinAnJiang2Model::P_FC_NAME = "FC";          // 稳定入渗率（毫米／小时）
const std::string XinAnJiang2Model::P_KKG_NAME = "KKG";      // 地下水退水系数KKG
const std::string XinAnJiang2Model::P_KSTOR_NAME = "KSTOR";  // day脉冲汇流计算的参数,Liang
const std::string XinAnJiang2Model::P_DT_NAME = "DT";        // 计算时段DT
const std::string XinAnJiang2Model::P_F_NAME = "F";          // 流域面积（km2）
//初始状态
const std::string XinAnJiang2Model::I_WU_NAME = "WU";        // 上下深层流域蓄水量（mm）
const std::string XinAnJiang2Model::I_WL_NAME = "WL";
const std::string XinAnJiang2Model::I_WD_NAME = "WD";  // 壤中流初始流量（m3/s）
const std::string XinAnJiang2Model::I_QRG0_NAME = "QRG0";    // 地下水初始流量（m3/s）
// input data name
const std::string XinAnJiang2Model::ID_P_NAME = "P";
const std::string XinAnJiang2Model::ID_EI_NAME = "EI";

XinAnJiang2Model::XinAnJiang2Model(std::map<std::string, double> &paraDatas, std::map<std::string, double> &initDatas) {
    this->paraDataNames.push_back(P_F_NAME);
    this->paraDataNames.push_back(P_DT_NAME);
    this->paraDataNames.push_back(P_WUM_NAME);
    this->paraDataNames.push_back(P_WLM_NAME);
    this->paraDataNames.push_back(P_WDM_NAME);
    this->paraDataNames.push_back(P_K_NAME);
    this->paraDataNames.push_back(P_C_NAME);
    this->paraDataNames.push_back(P_B_NAME);
    this->paraDataNames.push_back(P_IMP_NAME);
    this->paraDataNames.push_back(P_KKG_NAME);
    this->paraDataNames.push_back(P_FC_NAME);
    this->paraDataNames.push_back(P_KSTOR_NAME);
    if (!checkParaDatas(paraDatas)) {
        // todo
        exit(-1);
    }
    updateMapDatas(this->paraDatas, paraDatas);

    // init initDatas
    this->initDataNames.push_back(I_WU_NAME);
    this->initDataNames.push_back(I_WL_NAME);
    this->initDataNames.push_back(I_WD_NAME);
    this->initDataNames.push_back(I_QRG0_NAME);
    if (!checkInitDatas(initDatas)) {
        // todo
        exit(-1);
    }
    updateMapDatas(this->initDatas, initDatas);

    // init inputDatas
    this->inputDataNames.push_back(ID_P_NAME);
    this->inputDataNames.push_back(ID_EI_NAME);
}

void XinAnJiang2Model::loadModelParaDatas(std::string filepath) {
    std::ifstream fin(filepath);  // todo filepath is not existed
    std::string headName;
    std::string modelName;
    double headValue;
    fin >> headName >> modelName;
    if (modelName != "xinanjiang2") {
        // todo
    }
    while (fin >> headName >> headValue) {
        this->paraDatas[headName] = headValue; // todo check headName is belong to this model
    }
    fin.close();
}

void XinAnJiang2Model::loadModelInitDatas(std::string filepath) {
    std::ifstream fin(filepath);  // todo filepath is not existed
    std::string headName;
    std::string modelName;
    double headValue;
    fin >> headName >> modelName;
    if (modelName != "xinanjiang2") {
        // todo
    }
    while (fin >> headName >> headValue) {
        this->initDatas[headName] = headValue; // todo check headName is belong to this model
    }
    fin.close();
}

void XinAnJiang2Model::loadModelInputDatas(std::string filepath) {
    std::ifstream fin(filepath);  // todo filepath is not existed
    std::string headName;
    std::string modelName;
    fin >> headName >> modelName;
    fin >> headName >> modelName;
    fin >> headName >> modelName;

    std::string TMName, DRPName, EIName, TMValue;
    double DRPValue, EIValue;
    fin >> TMName >> DRPName >> EIName;
    utils::_type_time_step timeStep = 1;
    while (fin >> TMValue >> DRPValue >> EIValue) {
        std::map<std::string, double> values;
        values[DRPName] = DRPValue;
        values[EIName] = EIValue;
        this->inputDatas[timeStep] = values;
        timeStep++;
    }
//    printf("%d\n", timeStep);
    fin.close();
}

void XinAnJiang2Model::produceFlowSimul(std::map<std::string, double> &inputDatas, int nowTimestep) {
    // 模型的状态变量
    double PE;  // 大于零时为净雨量，小于零时为蒸发不足量，单位（毫米）

    double R;   // 产流深度，包括地表径流深度和地下径流深度两部分（毫米）
    double RG;  // 地下径流深度，单位（毫米）
    double RS;  // 地表径流深度，单位（毫米）

    double A;   // 当流域内的土壤湿度为W是,土壤含水量折算成的径流深度,单位（毫米）

    double E = 0.0;   // 蒸散发
    double EU = 0.0;   // 上层土壤蒸散发量（毫米）
    double EL = 0.0;   // 下层土壤蒸散发量（毫米）
    double ED = 0.0;   // 深层土壤蒸散发量（毫米）

    double W = this->initDatas[I_WU_NAME] + this->initDatas[I_WL_NAME] + this->initDatas[I_WD_NAME];   // 流域内土壤湿度
    double WU = this->initDatas[I_WU_NAME];  // 流域内上层土壤湿度
    double WL = this->initDatas[I_WL_NAME];  // 流域内下层土壤适度
    double WD = this->initDatas[I_WD_NAME];  // 流域内深层土壤湿度

    double WM = paraDatas[P_WUM_NAME] + paraDatas[P_WLM_NAME] + paraDatas[P_WDM_NAME];
    double WMM = WM * (1.0 + paraDatas[P_B_NAME]) / (1.0 - paraDatas[P_IMP_NAME]);

    PE = inputDatas[ID_P_NAME] - paraDatas[P_K_NAME] * inputDatas[ID_EI_NAME];

    // 如果降水量小于足蒸发需求
    if (PE < 0) {
        R = 0.0;    // 产流总量为零
        RG = 0.0;    // 地下径流量为零
        RS = 0.0;    // 地表径流量为零

        // 如果上层土壤含水量大于蒸发不足量
        if ((WU + PE) > 0.0) {
            // 上层土壤为流域蒸散发提供水量
            EU = paraDatas[P_K_NAME] * inputDatas[ID_EI_NAME];
            // 没有降水量用于增加土壤湿度
            EL = 0.0;          /* 降水用来增加土壤湿度的部分 */
            //
            ED = 0.0;
            // 更新上层土壤含水量
            WU = WU + PE;
        }
            // 上层土壤含水量小于蒸发不足量
        else {
            EU = WU + inputDatas[ID_P_NAME];        // 上层土壤蒸发,降水全部用于蒸发
            WU = 0.0;                 // 上层含水量为0，全部水分被蒸发
            // 如果下层含水量大于下层土壤的蒸散发潜力
            if (WL > (paraDatas[P_C_NAME] * paraDatas[P_WLM_NAME])) {
                EL = (paraDatas[P_K_NAME] * inputDatas[ID_EI_NAME] - EU) * (WL / paraDatas[P_WLM_NAME]);
                WL = WL - EL;
                ED = 0;
            }
                // 如果下层土壤含水量小于下层土壤的蒸散发潜力
            else {
                // 如果下层土壤的含水量蒸发之后还有剩余
                if (WL > paraDatas[P_C_NAME] * (paraDatas[P_K_NAME] * inputDatas[ID_EI_NAME] - EU)) {
                    EL = paraDatas[P_C_NAME] * (paraDatas[P_K_NAME] * inputDatas[ID_EI_NAME] - EU);
                    WL = WL - EL;/////////////////////////////////
                    ED = 0.0;
                }
                    // 如果下层土壤含水量全部蒸发之后尚不能满足蒸发需求
                else {
                    EL = WL;              /* 下层土壤含水量全部用于蒸散发 */
                    WL = 0.0;                /* 下层土剩余壤含水量为0        */
                    ED = paraDatas[P_C_NAME] * (paraDatas[P_K_NAME] * inputDatas[ID_EI_NAME] - EU) -
                         EL; /* 深层土壤含水量参与蒸发 */
                    WD = WD - ED;           /* 深层土壤含水量更新 */
                }
            }
        }
    }
        // 如果降水量大于或者等于蒸散发需求,即降水满足蒸发后还有剩余
    else {
        /*************** 以下代码负责径流划分计算 **************/
        // 初始化变量
        R = 0.0;    // 产流总量为零
        RG = 0.0;   // 地下径流产流深度为零
        RS = 0.0;   // 地表径流产流深度为零

        // 计算流域当天土壤含水量折算成的径流深度Ａ
        // m_WM:流域平均蓄水容量(一个参数),
        // m_W:流域内土壤湿度(一个状态变量)
        // B:蓄水容量曲线的方次(一个参数)
        A = WMM * (1 - pow((1.0 - W / WM), 1.0 / (1 + paraDatas[P_B_NAME])));
        // 土壤湿度折算净雨量加上降水后蒸发剩余雨量小于流域内最大含水容量
        if ((A + PE) < WMM) {
            // 流域内的产流深度计算
            R = PE             /* 降水蒸发后的剩余量(PE=P-E:状态变量) */
                + W          /* 流域内土壤湿度 (W:状态变量)         */
                + WM * pow((1 - (PE + A) / WMM), (1 + paraDatas[P_B_NAME]))
                - WM;      /* 减去流域平均蓄水容量（m_WM:参数）   */
        }
            // 土壤湿度折算净雨量加上降水后蒸发剩余雨量大于流域内最大含水容量
        else {
            // 流域内的产流深度计算
            R = PE             /* 降水蒸发后的剩余量(PE=P-E:状态变量) */
                + W          /* 流域内土壤湿度 (W:状态变量)         */
                - WM;      /* 减去流域平均蓄水容量（m_WM:参数）   */
        }
        // 如果降水蒸发后的剩余量小于土壤的稳定入渗率(m_FC:参数)
        if (PE > paraDatas[P_FC_NAME]) {
            // 计算地下径流的产流深度
            RG = (R - paraDatas[P_IMP_NAME] * PE) / PE * paraDatas[P_FC_NAME];
            // 计算地表径流的产流深度
            RS = R - RG;
        }
            // 如果降水经过蒸散发后的剩余量大于等于土壤稳定入渗率
        else {
            // 计算地下径流的产流深度
            RG = R -                /* 总产流深度                         */
                 paraDatas[P_IMP_NAME] * PE;        /* 不透水面积上的产流深度，IMP:参数 */
            // 计算地表径流的产流深度
            RS = R - RG;
        }
        /***************      径流划分计算结束      **************/

        // 计算上层土壤蒸散发量
        EU = paraDatas[P_K_NAME] *                 /* 流域蒸散发能力与实测蒸散发值的比 */
             inputDatas[ID_EI_NAME];           /* 当前时段的水面蒸发               */
        ED = 0.0;
        EL = 0.0;    /* 降水用来增加土壤湿度的部分 */

        /*************** 以下代码负责土壤含水量的更新计算 **************/
        // 如果上层土壤含水量与降水蒸散发剩余量之和减去产流量之后
        // 大于上层土壤的蓄水能力
        if ((WU + PE - R) >= paraDatas[P_WUM_NAME]) {
            // 上层含水量+下层含水量+降水剩余量-产流量-上层土壤蓄水需求
            // 后的水量大于下层土壤蓄水需求，多余水量分配到深层土壤
            if ((WU + WL + PE - R - paraDatas[P_WUM_NAME]) > paraDatas[P_WLM_NAME]) {
                WU = paraDatas[P_WUM_NAME];                 /* 上层土壤含水量=上层土壤蓄水容量 */
                WL = paraDatas[P_WLM_NAME];                 /* 下层土壤含水量=下层土壤蓄水容量 */
                WD = W + PE - R - WU - WL;          /* 绝对降水剩余量补充到深层土壤中  */
            }
                // 上层含水量+下层含水量+降水剩余量-产流量-上层土壤蓄水需求
                // 后的水量小于下层土壤蓄水需求，剩余水量补充到下层土壤中
            else {
                WL = WU + WL + PE - R - paraDatas[P_WUM_NAME];      /* 下层土壤含水量           */
                WU = paraDatas[P_WUM_NAME];                 /* 上层土壤蓄水容量得到满足 */
            }
        }
            // 如果上层土壤含水量与降水蒸散发剩余量之和减去产流量之后
            // 小于上层土壤的蓄水能力
        else {
            WU = WU + PE - R;
            // WU 可能小于零，应该加一些控制代码..........
        }
        /*************** 土壤含水量的更新计算结束 **************/
    }

    E = EU + EL + ED;
    W = WU + WL + WD;

    this->stateNowRS = RS;
    this->stateNowRG = RG;

    updateInitValue(I_WU_NAME, WU);
    updateInitValue(I_WL_NAME, WL);
    updateInitValue(I_WD_NAME, WD);

//
//    /* 以下部分是状态量：总蒸发量、上、下和深层土壤的蒸发的保存 */
///* 1 */    this->E[i] = E;     // 当前步长的蒸发        （模型重要输出）
///* 2 */    this->EU[i] = EU;   // 当前步长上层土壤蒸发
///* 3 */    this->EL[i] = EL;   // 当前步长下层土壤蒸发
///* 4 */    this->ED[i] = ED;   // 当前步长深层土壤蒸发
///* 5 */    this->W[i] = W;     // 当前步长流域平均土壤含水量
///* 6 */    this->WU[i] = WU;   // 当前步长流域上层土壤含水量
///* 7 */    this->WL[i] = WL;   // 当前步长流域下层土壤含水量
///* 8 */    this->WD[i] = WD;   // 当前步长流域深层土壤含水量
///* 9 */    this->RG[i] = RG;   // 当前步长流域基流径流深度
///* 10*/    this->RS[i] = RS;   // 当前步长流域地表径流径流深度
///* 11*/ this->R[i] = R;     // 当前步长的总产流径流深度

}

RoutingDataMeta XinAnJiang2Model::routeFlowSimul(std::map<std::string, double> &inputDatas, int nowTimestep,
                                                 RoutingDataMeta &upRoutedDatas) {
    // ------汇流计算------%
    // 地表径流
    double D = 24 / (getParaValue(P_DT_NAME)); //一日内时段数
    double K = getParaValue(P_KSTOR_NAME) * D;  // todo

    // 单位线推导
    double sum_UH = 0.0;
    std::vector<double> UH;
    int i = 0;
    while (true) {
        double uh = (1.0 / K) * exp((-1.0 * i) / K);
        if (uh >= 1.0) {
            UH.push_back(1.0);
            break;
        }
        if (sum_UH + uh > 1) {
            uh = 1.0 - sum_UH;
            UH.push_back(uh);
            break;
        } else {
            sum_UH = sum_UH + uh;
            UH.push_back(uh);
        }
        i++;
    }

    // upstream todo
    double upRS = upRoutedDatas.getFlow() * getParaValue(P_DT_NAME) * 3600 / (getParaValue(P_F_NAME) * 1000);

    // 单位线汇流计算
    if (nowTimestep <= UH.size()) {
        v_RS_tmp.push_back(stateNowRS + upRS);
    } else {
        v_RS_tmp.push_back(stateNowRS + upRS);
        v_RS_tmp.pop_front();
    }

    double QRS = 0;
    i = UH.size() - 1;
    for (std::list<double>::iterator it = v_RS_tmp.begin(); it != v_RS_tmp.end(); it++) {
        double q = getParaValue(P_F_NAME) * UH[i] / 3.6 / getParaValue(P_DT_NAME);
        QRS += *it * q;
        i--;
    }

    double QRG;
    QRG = getInitValue(I_QRG0_NAME) * pow(getParaValue(P_KKG_NAME), (1 / D)) +
          stateNowRG * (1 - pow(getParaValue(P_KKG_NAME), (1 / D))) * getParaValue(P_F_NAME) / 3.6 /
          getParaValue(P_DT_NAME);

    updateInitValue(I_QRG0_NAME, QRG);

    RoutingDataMeta routingDataMeta;
    routingDataMeta.setFlow(QRS);
    return routingDataMeta;
}

XinAnJiang2Model::XinAnJiang2Model() {

}
