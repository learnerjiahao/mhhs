//
// Created by wujiahao on 2018/11/24.
//

#include "hims_runoff_model.h"
#include "../model_warehouse.h"
#include "../../utils/datetime_parser.h"
#include <iostream>
#include <cmath>

std::vector<std::string> HIMSRunoffModel::getParaNames(const ModelContext *pModelContext) {
    return {"SMSC", "e", "R", "r",
            "La", "KG", "KC", "KB",
            "KI", "KKG", "KKSS"};
}

std::vector<std::string> HIMSRunoffModel::getInitNames(const ModelContext *pModelContext) {
    return {"SWC0", "GWs0", "QRS0", "QRSS0", "QRG0"};
}

std::vector<std::string> HIMSRunoffModel::getInputNames(const ModelContext *pModelContext) {
    return  {"P", "maxT", "minT"};
}

RoutingDataMeta
HIMSRunoffModel::runModel(ModelContext &pModelContext, const Config &configValues, const RoutingDataMeta &upRoutDatas,
                          int nowTimeStep) {
    double preSWC; // 非饱和土壤含水量
    double preGWs; // 地下水蓄水量
    double preQRS, preQRSS, preQRG; // 地表径流流量, 壤中流流量, 地下径流流量
    if (nowTimeStep == 0) {
        preSWC = SWC0;
        preGWs = GWs0;
        preQRS = QRS0;
        preQRSS = QRSS0;
        preQRG = QRG0;
    } else {
        preSWC = SWC;
        preGWs = GWs;
        preQRS = QRS;
        preQRSS = QRSS;
        preQRG = QRG;
    }

    double P = Ps[nowTimeStep];
    double maxT = maxTs[nowTimeStep];
    double minT = minTs[nowTimeStep];
    unsigned J = DatetimeParser::getDayOfYear(configValues.start_time, nowTimeStep,
                                              configValues.time_stride,
                                              DatetimeParser::DEFAULT_DATETIME_FORMAT) + 1; // 一年中的天数J

    double lati_rad = 3.1415 * pModelContext.latitude / 180;      // 换算纬度值φ(rad)=3.1415*G4/180

    // C4=P D4=maxT E4=minT
    // F4=J G4=lati H4=lati_rad
    // I4=SMSC J4=e K4=R L4=r M4=La N4=KG O4=KC P4=KB Q4=KI R4=KKG S4=KKSS T4=DT
    // U4=dr V4=q W4=ws X4=Ra Y4=ET0 Z4=ET AA4=ETa AB4=PE AC4=f AD4=Qd AE4=Ql AF4=SWC AG4=Rs AH4=REC AI4=GWs AJ4=Qb AK4=amendf AL4=amendREC
    // AM4=QRS AN4=QRSS AO4=QRG AP4=Q

    // 地球太阳逆相对距离dr
    double dr = 1 + 0.033 * cos((2 * 3.1415 * J) / 365);
    // 太阳辐射消能角δ
    double q = 0.409 * sin((2 * 3.1415 * J) / 365 - 1.39);
    // 落日角ѡs
    double ws = acos(-tan(lati_rad) * tan(q));
    // 大气层顶辐射Ra(MJ/(m2·day))
    double Ra = (24 / 3.1415) * 0.082 * dr * (ws * sin(lati_rad) * sin(q) + cos(lati_rad) * cos(q) * sin(ws));
    // 潜在蒸散发ET0
    double ET0 = 0.0023 * (Ra / 2.45) * sqrt(maxT - minT) * (((maxT + minT) / 2) + 17.8);
    // Z4=ET
    double ET = ET0 * (1 - pow((1 - preSWC / SMSC), e));
    // AA4=ETa
    double ETa = ET * KC;
    // AB4=PE
    double PE = P - ETa;
    // AC4=f
//    double f=IF(PE>0,R*POWER(PE,r),0);
    double f;
    if (PE > 0) {
        f = R * pow(PE, r);
    } else {
        f = 0;
    }
    // AD4=Qd
//    double Qd =IF(PE>0,IF((PE-f)>0,(PE-f),0),0);
    double Qd;
    if (PE > 0) {
        if ((PE - f) > 0) {
            Qd = (PE - f);
        } else {
            Qd = 0;
        }

    } else {
        Qd = 0;
    }
    // AE4=Ql
//    double Ql =La*(AF3/SMSC)*f
    double Ql = La * (preSWC / SMSC) * f;

    // AH4=REC
    double REC = KG * (preSWC / SMSC) * (f - Ql);

    // AF4=SWC
    //SWC =IF(PE>0,IF((f+preSWC-Ql-REC)>=SMSC,SMSC,(f+preSWC-Ql-REC)),IF((preSWC-Ql-REC+PE)>=SMSC,SMSC,(preSWC-Ql-REC+PE)));
    if (PE > 0) {
        if ((f + preSWC - Ql - REC) >= SMSC) {
            SWC = SMSC;
        } else {
            SWC = (f + preSWC - Ql - REC);
        }
    } else {
        if ((preSWC - Ql - REC + PE) >= SMSC) {
            SWC = SMSC;
        } else {
            SWC = (preSWC - Ql - REC + PE);
        }
    }

    // AG4=Rs
    //double Rs =IF(SWC>SMSC,SWC-SMSC,0);
    double Rs;
    if (SWC > SMSC) {
        Rs = SWC - SMSC;
    } else {
        Rs = 0;
    }

    // AJ4=Qb
//    double Qb =KB*(REC+AI3);
    double Qb = KB * (REC + preGWs);

    // AI4=GWs
    GWs = preGWs + REC - Qb;

    // AK4=amendf =IF(PE>0,f-(Ql+REC+SWC-preSWC),0)
    double amendf;
    if (PE > 0) {
        amendf = f - (Ql + REC + SWC - preSWC);
    } else {
        amendf = 0;
    }

    // AL4=amendREC =AH4-(AJ4+AI4-AI3)
    double amendREC = REC - (Qb + GWs - preGWs); //


    // AM4=QRS  =Q4*AM3+(1-Q4)*(AD4+AG4)*(31871.6/(3.6*T4))
    QRS = KI * preQRS +
          (1 - KI) * (Qd + Rs) * (pModelContext.area / (3.6 * configValues.time_stride));
    // AN4=QRSS  =S4*AN3+(1-S4)*AE4*(31871.6/(3.6*T4))
    QRSS = KKSS * preQRSS +
           (1 - KKSS) * Ql * (pModelContext.area / (3.6 * configValues.time_stride));
    // AO4=QRG  =R4*AO3+(1-R4)*AJ4*(31871.6/(3.6*T4))
    QRG = KKG * preQRG +
          (1 - KKG) * Qb * (pModelContext.area / (3.6 * configValues.time_stride));
    // AP4=Q
    double Q = QRS + QRSS + QRG;

    pModelContext.res_flow = Q;

    return RoutingDataMeta(pModelContext.res_flow);
}

HIMSRunoffModel::HIMSRunoffModel(ModelContext *pModelContext) :
        BaseModel(pModelContext),
        SMSC(pModelContext->getParamData("SMSC")),
        e(pModelContext->getParamData("e")),
        R(pModelContext->getParamData("R")),
        r(pModelContext->getParamData("r")),
        La(pModelContext->getParamData("La")),
        KG(pModelContext->getParamData("KG")),
        KC(pModelContext->getParamData("KC")),
        KB(pModelContext->getParamData("KB")),
        KI(pModelContext->getParamData("KI")),
        KKG(pModelContext->getParamData("KKG")),
        KKSS(pModelContext->getParamData("KKSS")),
        //状态
        SWC0(pModelContext->getInitData("SWC0")),
        GWs0(pModelContext->getInitData("GWs0")),
        QRS0(pModelContext->getInitData("QRS0")),    // 地下水初始流量（m3/s）
        QRSS0(pModelContext->getInitData("QRSS0")),  // 壤中流初始流量（m3/s）
        QRG0(pModelContext->getInitData("QRG0")),    // 地下水初始流量（m3/s）

        // input data
        Ps(pModelContext->getInputData("P")),
        maxTs(pModelContext->getInputData("maxT")),
        minTs(pModelContext->getInputData("minT")){}
