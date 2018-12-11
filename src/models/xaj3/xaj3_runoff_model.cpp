//
// Created by parallels on 11/28/18.
//

#include <cmath>
#include "xaj3_runoff_model.h"

RoutingDataMeta XAJ3RunoffModel::runModel(ModelContext &pModelContext, const Config &configValues,
                                          const RoutingDataMeta &upRoutDatas, int nowTimeStep) {
    double EI = this->EIs[nowTimeStep-1];
    double P = this->Ps[nowTimeStep-1];
    
    double W = this->WU + this->WL + this->WD; //WU + WL + WD;
    double WM = this->WUM + this->WLM + this->WDM; //WUM + WLM + WDM;
    double WWMM = (1 + this->B) * WM;
    double A = WWMM * (1 - pow((1 - W / WM), (1 / (1 + this->B))));

    double EM = this->K * EI; //蒸散发能力
    double PE = P - EM; //计算净雨量

    //计算进入自由水库的水量R
    double R;
    if (PE > 0) {
        if (PE + A >= WWMM) {
            R = PE - (WM - W);
        } else {
            R = PE - ((WM - W) - WM * pow((1 - (PE + A) / WWMM), (1 + this->B)));
        }
    } else {
        R = 0;
    }

    //计算蒸发
    double E;
    double WU1 = this->WU + P; //随着降雨流域蓄水量从表层发生变化，同时也有蒸发的存在
    double EU, EL, ED;

    if (WU1 >= EM) {
        EU = EM;
        EL = 0;
        ED = 0;
    } else {
        EU = WU1;
        if (this->C <= this->WL / this->WLM) {
            EL = (EM - EU) * this->WL / this->WLM;
            ED = 0;
        } else if (this->C <= this->WL / (EM - EU)) {
            EL = this->C * (EM - EU);
            ED = 0;
        } else {
            EL = this->WL;
            ED = this->C * (EM - EU) - EL;
        }
    }
    E = EU + EL + ED;

    //利用自由水蓄水容量曲线计算产流量
    double RS, RSS, RG;
    double SSM = (1 + this->EX) * this->SM; //域上自由水蓄水量最大的某点的蓄量值
    double AU = SSM * (1 - pow((1 - this->S / this->SM),
                               (1 / (1 + this->EX)))); //与自由水蓄水量S对应的蓄水容量曲线的纵坐标值

    if (PE <= 0) {
        RS = 0;
        RSS = this->S * this->KSS * this->FR; //%FR最初采用初始值
        RG = this->S * this->KG * this->FR;
        this->S = (1 - this->KSS - this->KG) * this->S;
    } else {
        if (R / PE > 1) { //产流面积比重等效于径流系数
            this->FR = 1;
        } else {
            this->FR = R / PE;
        }

        if (PE + AU < SSM) {
            RS = (PE - this->SM + this->S +
                  this->SM * pow((1 - (PE + AU) / SSM), (1 + this->EX))) *
                 this->FR;
            RSS = (this->SM -
                   this->SM * pow((1 - (PE + AU) / SSM), (1 + this->EX))) *
                  this->KSS * this->FR;
            RG = (this->SM -
                  this->SM * pow((1 - (PE + AU) / SSM), (1 + this->EX))) *
                 this->KG * this->FR;
            this->S = (1 - this->KSS - this->KG) *
                      (this->SM - this->SM *
                                  pow((1 - (PE + AU) / SSM),
                                      (1 + this->EX)));
        } else {
            RS = (PE - this->SM + this->S) * this->FR;
            RSS = this->SM * this->KSS * this->FR;
            RG = this->SM * this->KG * this->FR;
            this->S =
                    (1 - this->KSS - this->KG) * this->SM;
        }
    }
    RS = P * this->IMP + RS * (1 - this->IMP); //考虑不透水面
    this->FR = 1 - pow((1 - W / WM), (this->B / (1 + this->B)));

    //计算蓄水量的变化
    this->WU = this->WU + P - R - EU;
    if (this->WU >= this->WUM) {
        this->WL =
                this->WL - EL + (this->WU - this->WUM);
        if (this->WL >= this->WLM) {
            this->WD =
                    this->WD - ED + (this->WL - this->WLM);
            if (this->WD >= this->WDM) {
                this->WD = this->WDM;
            }
            this->WL = this->WLM;
        }
        this->WU = this->WUM;
    }
    W = this->WU + this->WL + this->WD;
    stateNowRS = RS;
    stateNowRSS = RSS;
    stateNowRG = RG;

    // ------汇流计算------%
    // 地表径流
    double D = 24 / (configValues.time_stride); //一日内时段数
    double K = KSTOR * D;

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

    // 单位线汇流计算
    if (nowTimeStep-1 <= UH.size()) {
        v_RS_tmp.push_back(stateNowRS);
    } else {
        v_RS_tmp.push_back(stateNowRS);
        v_RS_tmp.pop_front();
    }

    double QRS = 0;
    i = UH.size() - 1;
    for (std::list<double>::iterator it = v_RS_tmp.begin(); it != v_RS_tmp.end(); it++) {
        double q = pModelContext.area * UH[i] / 3.6 / configValues.time_stride;
        QRS += *it * q;
        i--;
    }

    double QRG, QRSS;
    if (nowTimeStep == 1) {
        QRG = this->QRG0 * pow(this->KKG, (1 / D)) +
              stateNowRG * (1 - pow(this->KKG, (1 / D))) * pModelContext.area / 3.6 /
              configValues.time_stride;
        QRSS = this->QRG0 * pow(this->KKSS, (1 / D)) +
               stateNowRSS * (1 - pow(this->KKSS, (1 / D))) * pModelContext.area / 3.6 /
               configValues.time_stride;
    } else {
        QRG = statePrevQRG * pow(this->KKG, (1 / D)) +
              stateNowRG * (1 - pow(this->KKG, (1 / D))) * pModelContext.area / 3.6 /
              configValues.time_stride;
        QRSS = statePrevQRSS * pow(this->KKSS, (1 / D)) +
               stateNowRSS * (1 - pow(this->KKSS, (1 / D))) * pModelContext.area / 3.6 /
               configValues.time_stride;
    }

    statePrevQRSS = QRSS;
    statePrevQRG = QRG;

    pModelContext.res_flow = QRS + QRSS + QRG;
    return RoutingDataMeta(QRS + QRSS + QRG);
}

XAJ3RunoffModel::XAJ3RunoffModel(ModelContext *pModelContext) :
        BaseModel(pModelContext),
        WUM(pModelContext->getParamData("WUM")),
        WLM(pModelContext->getParamData("WLM")),
        WDM(pModelContext->getParamData("WDM")),
        K(pModelContext->getParamData("K")),          // 蒸发皿系数
        C(pModelContext->getParamData("C")),          // 深层蒸散发系数
        B(pModelContext->getParamData("B")),          // 蓄水容量曲线指数
        IMP(pModelContext->getParamData("IMP")),      // 不透水面积比重
        SM(pModelContext->getParamData("SM")),        // 自由水蓄水库容量（mm）
        EX(pModelContext->getParamData("EX")),        // 自由水蓄水容量曲线指数
        KG(pModelContext->getParamData("KG")),        // 地下水从自由水库的出流系数
        KSS(pModelContext->getParamData("KSS")),      // 壤中流出流系数KSS
        KKG(pModelContext->getParamData("KKG")),      // 地下水退水系数KKG
        KKSS(pModelContext->getParamData("KKSS")),    // 壤中流退水系数KKSS
        KSTOR(pModelContext->getParamData("KSTOR")),  // day脉冲汇流计算的参数,Liang
        //状态
        WU(pModelContext->getInitData("WU0")),        // 上下深层流域蓄水量（mm）
        WL(pModelContext->getInitData("WL0")),
        WD(pModelContext->getInitData("WD0")),
        FR(pModelContext->getInitData("FR0")),        // 产流面积比重
        S(pModelContext->getInitData("S0")),          // 自由水蓄水量（mm）
        QRSS0(pModelContext->getInitData("QRSS0")),  // 壤中流初始流量（m3/s）
        QRG0(pModelContext->getInitData("QRG0")),    // 地下水初始流量（m3/s）
        // input data
        Ps(pModelContext->getInputData("P")),
        EIs(pModelContext->getInputData("EI")) {

//    paraNames = {"F", "WUM", "WLM", "WDM",  "K", "C", "B", "IMP", "SM",
//                 "EX", "KG", "KSS", "KKG", "KKSS", "KSTOR"};
//    initNames = {"WU0", "WL0", "WD0", "FR0", "S0", "QRSS0", "QRG0"};
//    inputNames = {"P", "EI"};
}

std::vector<std::string> XAJ3RunoffModel::getParaNames(const ModelContext *pModelContext) {
    return {"F", "WUM", "WLM", "WDM",  "K", "C", "B", "IMP", "SM",
                 "EX", "KG", "KSS", "KKG", "KKSS", "KSTOR"};
}

std::vector<std::string> XAJ3RunoffModel::getInitNames(const ModelContext *pModelContext) {
    return {"WU0", "WL0", "WD0", "FR0", "S0", "QRSS0", "QRG0"};
}

std::vector<std::string> XAJ3RunoffModel::getInputNames(const ModelContext *pModelContext) {
    return {"P", "EI"};
}
