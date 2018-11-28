//
// Created by wujiahao on 18-6-19.
//

#include <fstream>
#include "xinanjiang3_model.h"

//基本参数
const std::string XinAnJiang3Model::P_F_NAME = "F";          // 流域面积（km2）
const std::string XinAnJiang3Model::P_WUM_NAME = "WUM";      // 上下深层流域蓄水容量（mm）
const std::string XinAnJiang3Model::P_WLM_NAME = "WLM";
const std::string XinAnJiang3Model::P_WDM_NAME = "WDM";
const std::string XinAnJiang3Model::P_K_NAME = "K";          // 蒸发皿系数
const std::string XinAnJiang3Model::P_C_NAME = "C";          // 深层蒸散发系数
const std::string XinAnJiang3Model::P_B_NAME = "B";          // 蓄水容量曲线指数
const std::string XinAnJiang3Model::P_IMP_NAME = "IMP";      // 不透水面积比重
const std::string XinAnJiang3Model::P_SM_NAME = "SM";        // 自由水蓄水库容量（mm）
const std::string XinAnJiang3Model::P_EX_NAME = "EX";        // 自由水蓄水容量曲线指数
const std::string XinAnJiang3Model::P_KG_NAME = "KG";        // 地下水从自由水库的出流系数
const std::string XinAnJiang3Model::P_KSS_NAME = "KSS";      // 壤中流出流系数KSS
const std::string XinAnJiang3Model::P_KKG_NAME = "KKG";      // 地下水退水系数KKG
const std::string XinAnJiang3Model::P_KKSS_NAME = "KKSS";    // 壤中流退水系数KKSS
const std::string XinAnJiang3Model::P_DT_NAME = "DT";        // 计算时段DT
const std::string XinAnJiang3Model::P_KSTOR_NAME = "KSTOR";  // day脉冲汇流计算的参数,Liang
//初始状态
const std::string XinAnJiang3Model::I_WU_NAME = "WU";        // 上下深层流域蓄水量（mm）
const std::string XinAnJiang3Model::I_WL_NAME = "WL";
const std::string XinAnJiang3Model::I_WD_NAME = "WD";
const std::string XinAnJiang3Model::I_FR_NAME = "FR";        // 产流面积比重
const std::string XinAnJiang3Model::I_S_NAME = "S";          // 自由水蓄水量（mm）
const std::string XinAnJiang3Model::I_QRSS0_NAME = "QRSS0";  // 壤中流初始流量（m3/s）
const std::string XinAnJiang3Model::I_QRG0_NAME = "QRG0";    // 地下水初始流量（m3/s）
// input data name
const std::string XinAnJiang3Model::ID_P_NAME = "P";
const std::string XinAnJiang3Model::ID_EI_NAME = "EI";

XinAnJiang3Model::XinAnJiang3Model(std::map<std::string, double> &paraDatas, std::map<std::string, double> &initDatas) {
    this->paraDataNames.push_back(P_F_NAME);
    this->paraDataNames.push_back(P_WUM_NAME);
    this->paraDataNames.push_back(P_WLM_NAME);
    this->paraDataNames.push_back(P_WDM_NAME);
    this->paraDataNames.push_back(P_K_NAME);
    this->paraDataNames.push_back(P_C_NAME);
    this->paraDataNames.push_back(P_B_NAME);
    this->paraDataNames.push_back(P_IMP_NAME);
    this->paraDataNames.push_back(P_SM_NAME);
    this->paraDataNames.push_back(P_EX_NAME);
    this->paraDataNames.push_back(P_KG_NAME);
    this->paraDataNames.push_back(P_KSS_NAME);
    this->paraDataNames.push_back(P_KKG_NAME);
    this->paraDataNames.push_back(P_KKSS_NAME);
    this->paraDataNames.push_back(P_DT_NAME);
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
    this->initDataNames.push_back(I_FR_NAME);
    this->initDataNames.push_back(I_S_NAME);
    this->initDataNames.push_back(I_QRSS0_NAME);
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

void XinAnJiang3Model::produceFlowSimul(std::map<std::string, double> &inputDatas, int nowTimestep) {

    double W = getInitValue(I_WU_NAME) + getInitValue(I_WL_NAME) + getInitValue(I_WD_NAME); //WU + WL + WD;
    double WM = getParaValue(P_WUM_NAME) + getParaValue(P_WLM_NAME) + getParaValue(P_WDM_NAME); //WUM + WLM + WDM;
    double WWMM = (1 + getParaValue(P_B_NAME)) * WM;
    double A = WWMM * (1 - pow((1 - W / WM), (1 / (1 + getParaValue(P_B_NAME)))));

    double EM = getParaValue(P_K_NAME) * inputDatas.at(ID_EI_NAME); //蒸散发能力
    double PE = inputDatas.at(ID_P_NAME) - EM; //计算净雨量

    //计算进入自由水库的水量R
    double R;
    if (PE > 0) {
        if (PE + A >= WWMM) {
            R = PE - (WM - W);
        } else {
            R = PE - ((WM - W) - WM * pow((1 - (PE + A) / WWMM), (1 + getParaValue(P_B_NAME))));
        }
    } else {
        R = 0;
    }

    //计算蒸发
    double E;
    double WU1 = getInitValue(I_WU_NAME) + inputDatas.at(ID_P_NAME); //随着降雨流域蓄水量从表层发生变化，同时也有蒸发的存在
    double EU, EL, ED;

    if (WU1 >= EM) {
        EU = EM;
        EL = 0;
        ED = 0;
    } else {
        EU = WU1;
        if (getParaValue(P_C_NAME) <= getInitValue(I_WL_NAME) / getParaValue(P_WLM_NAME)) {
            EL = (EM - EU) * getInitValue(I_WL_NAME) / getParaValue(P_WLM_NAME);
            ED = 0;
        } else if (getParaValue(P_C_NAME) <= getInitValue(I_WL_NAME) / (EM - EU)) {
            EL = getParaValue(P_C_NAME) * (EM - EU);
            ED = 0;
        } else {
            EL = getInitValue(I_WL_NAME);
            ED = getParaValue(P_C_NAME) * (EM - EU) - EL;
        }
    }
    E = EU + EL + ED;

    //利用自由水蓄水容量曲线计算产流量
    double RS, RSS, RG;
    double SSM = (1 + getParaValue(P_EX_NAME)) * getParaValue(P_SM_NAME); //域上自由水蓄水量最大的某点的蓄量值
    double AU = SSM * (1 - pow((1 - getInitValue(I_S_NAME) / getParaValue(P_SM_NAME)),
                               (1 / (1 + getParaValue(P_EX_NAME))))); //与自由水蓄水量S对应的蓄水容量曲线的纵坐标值

    if (PE <= 0) {
        RS = 0;
        RSS = getInitValue(I_S_NAME) * getParaValue(P_KSS_NAME) * getInitValue(I_FR_NAME); //%FR最初采用初始值
        RG = getInitValue(I_S_NAME) * getParaValue(P_KG_NAME) * getInitValue(I_FR_NAME);
        updateInitValue(I_S_NAME,
                        (1 - getParaValue(P_KSS_NAME) - getParaValue(P_KG_NAME)) * getInitValue(I_S_NAME));
    } else {
        if (R / PE > 1) { //产流面积比重等效于径流系数
            updateInitValue(I_FR_NAME, 1);
        } else {
            updateInitValue(I_FR_NAME, R / PE);
        }

        if (PE + AU < SSM) {
            RS = (PE - getParaValue(P_SM_NAME) + getInitValue(I_S_NAME) +
                  getParaValue(P_SM_NAME) * pow((1 - (PE + AU) / SSM), (1 + getParaValue(P_EX_NAME)))) *
                 getInitValue(I_FR_NAME);
            RSS = (getParaValue(P_SM_NAME) -
                   getParaValue(P_SM_NAME) * pow((1 - (PE + AU) / SSM), (1 + getParaValue(P_EX_NAME)))) *
                  getParaValue(P_KSS_NAME) * getInitValue(I_FR_NAME);
            RG = (getParaValue(P_SM_NAME) -
                  getParaValue(P_SM_NAME) * pow((1 - (PE + AU) / SSM), (1 + getParaValue(P_EX_NAME)))) *
                 getParaValue(P_KG_NAME) * getInitValue(I_FR_NAME);
            updateInitValue(I_S_NAME, (1 - getParaValue(P_KSS_NAME) - getParaValue(P_KG_NAME)) *
                                      (getParaValue(P_SM_NAME) - getParaValue(P_SM_NAME) *
                                                                 pow((1 - (PE + AU) / SSM),
                                                                     (1 + getParaValue(P_EX_NAME)))));
        } else {
            RS = (PE - getParaValue(P_SM_NAME) + getInitValue(I_S_NAME)) * getInitValue(I_FR_NAME);
            RSS = getParaValue(P_SM_NAME) * getParaValue(P_KSS_NAME) * getInitValue(I_FR_NAME);
            RG = getParaValue(P_SM_NAME) * getParaValue(P_KG_NAME) * getInitValue(I_FR_NAME);
            updateInitValue(I_S_NAME,
                            (1 - getParaValue(P_KSS_NAME) - getParaValue(P_KG_NAME)) * getParaValue(P_SM_NAME));
        }
    }
    RS = inputDatas[ID_P_NAME] * getParaValue(P_IMP_NAME) + RS * (1 - getParaValue(P_IMP_NAME)); //考虑不透水面
    updateInitValue(I_FR_NAME, 1 - pow((1 - W / WM), (getParaValue(P_B_NAME) / (1 + getParaValue(P_B_NAME)))));

    //计算蓄水量的变化
    updateInitValue(I_WU_NAME, getInitValue(I_WU_NAME) + inputDatas.at(ID_P_NAME) - R - EU);
    if (getInitValue(I_WU_NAME) >= getParaValue(P_WUM_NAME)) {
        updateInitValue(I_WL_NAME,
                        getInitValue(I_WL_NAME) - EL + (getInitValue(I_WU_NAME) - getParaValue(P_WUM_NAME)));
        if (getInitValue(I_WL_NAME) >= getParaValue(P_WLM_NAME)) {
            updateInitValue(I_WD_NAME,
                            getInitValue(I_WD_NAME) - ED + (getInitValue(I_WL_NAME) - getParaValue(P_WLM_NAME)));
            if (getInitValue(I_WD_NAME) >= getParaValue(P_WDM_NAME)) {
                updateInitValue(I_WD_NAME, getParaValue(P_WDM_NAME));
            }
            updateInitValue(I_WL_NAME, getParaValue(P_WLM_NAME));
        }
        updateInitValue(I_WU_NAME, getParaValue(P_WUM_NAME));
    }
    W = getInitValue(I_WU_NAME) + getInitValue(I_WL_NAME) + getInitValue(I_WD_NAME);

    stateNowRS = RS;
    stateNowRSS = RSS;
    stateNowRG = RG;
}

RoutingDataMeta XinAnJiang3Model::routeFlowSimul(std::map<std::string, double> &inputDatas, int nowTimestep,
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

    double QRG, QRSS;
    if (nowTimestep == 1) {
        QRG = getInitValue(I_QRG0_NAME) * pow(getParaValue(P_KKG_NAME), (1 / D)) +
              stateNowRG * (1 - pow(getParaValue(P_KKG_NAME), (1 / D))) * getParaValue(P_F_NAME) / 3.6 /
              getParaValue(P_DT_NAME);
        QRSS = getInitValue(I_QRG0_NAME) * pow(getParaValue(P_KKSS_NAME), (1 / D)) +
               stateNowRSS * (1 - pow(getParaValue(P_KKSS_NAME), (1 / D))) * getParaValue(P_F_NAME) / 3.6 /
               getParaValue(P_DT_NAME);
    } else {
        QRG = statePrevQRG * pow(getParaValue(P_KKG_NAME), (1 / D)) +
              stateNowRG * (1 - pow(getParaValue(P_KKG_NAME), (1 / D))) * getParaValue(P_F_NAME) / 3.6 /
              getParaValue(P_DT_NAME);
        QRSS = statePrevQRSS * pow(getParaValue(P_KKSS_NAME), (1 / D)) +
               stateNowRSS * (1 - pow(getParaValue(P_KKSS_NAME), (1 / D))) * getParaValue(P_F_NAME) / 3.6 /
               getParaValue(P_DT_NAME);
    }

    statePrevQRSS = QRSS;
    statePrevQRG = QRG;

    RoutingDataMeta routingDataMeta;
    routingDataMeta.setFlow(QRS + QRSS);
    return routingDataMeta;
}

void XinAnJiang3Model::createModelParaDatas(std::map<std::string, double> &paraDatas, double F, double WUM, double WLM,
                                           double WDM, double K, double C, double B, double IMP, double SM, double EX,
                                           double KG, double KSS, double KKG, double KKSS, int DT, double KSTOR) {
    paraDatas[P_F_NAME] = F;
    paraDatas[P_WUM_NAME] = WUM;
    paraDatas[P_WLM_NAME] = WLM;
    paraDatas[P_WDM_NAME] = WDM;
    paraDatas[P_K_NAME] = K;
    paraDatas[P_C_NAME] = C;
    paraDatas[P_B_NAME] = B;
    paraDatas[P_IMP_NAME] = IMP;
    paraDatas[P_SM_NAME] = SM;
    paraDatas[P_EX_NAME] = EX;
    paraDatas[P_KG_NAME] = KG;
    paraDatas[P_KSS_NAME] = KSS;
    paraDatas[P_KKG_NAME] = KKG;
    paraDatas[P_KKSS_NAME] = KKSS;
    paraDatas[P_DT_NAME] = DT;
    paraDatas[P_KSTOR_NAME] = KSTOR;
}

void XinAnJiang3Model::createModelInitDatas(std::map<std::string, double> &initDatas, double WU, double WL, double WD,
                                           double FR, double S, double QRSS0, double QRG0) {
    initDatas[I_WU_NAME] = WU;
    initDatas[I_WL_NAME] = WL;
    initDatas[I_WD_NAME] = WD;
    initDatas[I_FR_NAME] = FR;
    initDatas[I_S_NAME] = S;
    initDatas[I_QRSS0_NAME] = QRSS0;
    initDatas[I_QRG0_NAME] = QRG0;
}

void XinAnJiang3Model::createModelInputDatas(std::map<std::string, double> &inputDatas, double P, double EI) {
    inputDatas[ID_P_NAME] = P;
    inputDatas[ID_EI_NAME] = EI;
}

void XinAnJiang3Model::loadModelParaDatas(std::string filepath) {
    std::ifstream fin(filepath);  // todo filepath is not existed
    std::string headName;
    std::string modelName;
    double headValue;
    fin >> headName >> modelName;
    if(modelName != "xinanjiang3") {
        // todo
    }
    while (fin >> headName >> headValue) {
        this->paraDatas[headName] = headValue; // todo check headName is belong to this model
    }
    fin.close();
}

void XinAnJiang3Model::loadModelInitDatas(std::string filepath) {
    std::ifstream fin(filepath);  // todo filepath is not existed
    std::string headName;
    std::string modelName;
    double headValue;
    fin >> headName >> modelName;
    if(modelName != "xinanjiang3") {
        // todo
    }
    while (fin >> headName >> headValue) {
        this->initDatas[headName] = headValue; // todo check headName is belong to this model
    }
    fin.close();
}

void XinAnJiang3Model::loadModelInputDatas(std::string filepath) {
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
    while(fin >> TMValue >> DRPValue >> EIValue) {
        std::map<std::string, double> values;
        values[DRPName] = DRPValue;
        values[EIName] = EIValue;
        this->inputDatas[timeStep] = values;
        timeStep ++;
    }
    fin.close();
}

XinAnJiang3Model::~XinAnJiang3Model() {

}

XinAnJiang3Model::XinAnJiang3Model() {

}

