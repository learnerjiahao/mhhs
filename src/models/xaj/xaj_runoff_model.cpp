//
// Created by wujiahao on 18-7-28.
//

#include <cmath>
#include <iostream>
#include <vector>
#include <fstream>
#include <map>
#include "xaj_runoff_model.h"
#include "../../utils/model_file_utils.h"
#include "../model_warehouse.h"
XAJRunoffModel::XAJRunoffModel(ModelContext *pModelContext):
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
            KI(pModelContext->getParamData("KI")),      // 壤中流出流系数KSS
            KKG(pModelContext->getParamData("KKG")),      // 地下水退水系数KKG
            KKSS(pModelContext->getParamData("KKSS")),    // 壤中流退水系数KKSS
            //状态
            initWU(pModelContext->getInitData("WU0")),        // 上下深层流域蓄水量（mm）
            initWL(pModelContext->getInitData("WL0")),
            initWD(pModelContext->getInitData("WD0")),
            initFR(pModelContext->getInitData("FR0")),          // 自由水蓄水量（mm）
            initS(pModelContext->getInitData("S0")),          // 自由水蓄水量（mm）
            initQRS(pModelContext->getInitData("QRS0")),    // 地下水初始流量（m3/s）
            initQRSS(pModelContext->getInitData("QRSS0")),  // 壤中流初始流量（m3/s）
            initQRG(pModelContext->getInitData("QRG0")),    // 地下水初始流量（m3/s）

            // input data
            Ps(pModelContext->getInputData("P")),
            EIs(pModelContext->getInputData("EI")) {

//        paraNames = {"WUM", "WLM", "WDM", "K", "C", "B", "IMP", "SM",
//                     "EX", "KG", "KSS", "KI", "KKG", "KKSS"};
//        initNames = {"WU0", "WL0", "WD0", "FR0", "S0", "QRS0", "QRSS0", "QRG0"};
//        inputNames = {"P", "EI"};
}

RoutingDataMeta XAJRunoffModel::runModel(ModelContext &pModelContext, const Config &configValues,
                                         const RoutingDataMeta &upRoutDatas, int nowTimeStep) {
    double EI = this->EIs[nowTimeStep-1];
    double P = this->Ps[nowTimeStep-1];

    double preWU, preWL, preWD, preFR, preS, preQRSS, preQRG, preQRS;  //前一时刻状态变量
    if (nowTimeStep == 1) {
//        preSR = initSR;
        preWU = initWU;
        preWL = initWL;
        preWD = initWD;
        preFR = initFR;
        preS = initS;
        preQRSS = initQRSS;
        preQRG = initQRG;
        preQRS = initQRS;
    } else {
//        preSR = nowSR;
        preWU = nowWU;
        preWL = nowWL;
        preWD = nowWD;
        preFR = nowFR;
        preS = nowS;
        preQRSS = nowQRSS;
        preQRG = nowQRG;
        preQRS = nowQRS;
    }


    // P=B4 EI=C4 EM=D4 WUM=E4 WLM=F4 WDM=G4 KC=H4 C=I4 B=J4 IMP=K4 SM=L4 EX=M4 KG=N4 KSS=O4 KI=P4 KKG=Q4 KKSS=R4
    // EU=T4 EL=U4 ED=V4 E=W4 PE=X4 W=Y4 WM=Z4 WMM=AA4 A=AB4 R=AC4 WU=AD4 WL=AE4 WD=AF4 MS=AG4 AU=AH4 FR=AI4 SR=AJ4 S=AK4
    // RS=AL4 RI=AM4 RG=AN4 RS1=AO4 RI1=AP4 RG1=AQ4
    // QRS=AR4 QRSS=AS4 QRG=AT4 Q=AU4

    // 1. 蓄满产流模型
    double EM = EI * this->K;  // 实际蒸发量EM(mm)
    double EU;  // 上层的时段蒸散发量EU（mm）EU=IF((AD3+B4)>=D4,D4,AD3+B4)
    // EU=IF((WU0+P)>=EM,EM,WU0+P)
    if (preWU + P >= EM) {
        EU = EM;
    } else {
        EU = preWU + P;
    }
    double EL; // 下层的时段蒸散发量EL（mm）EL=IF((AD3+B4)>=D4,0,IF(AE3>=(I4*F4),(D4-T4)*AE3/F4,IF(AE3>=(I4*(D4-T4)),I4*(D4-T4),AE3)))
    // EL=IF((WU0+P)>=EM,0,IF(WL0>=(C*WLM),(EM-EU)*WL0/WLM,IF(WL0>=(C*(EM-EU)),C*(EM-EU),WL0)))
    if (preWU + P >= EM) {
        EL = 0.0;
    } else {
        if (preWL >= this->C * this->WLM) {
            EL = (EM - EU) * preWL / this->WLM;
        } else {
            if (preWL >= (this->C * (EM - EU))) {
                EL = this->C * (EM - EU);
            } else {
                EL = preWL;
            }
        }
    }
    double ED; // 深层的时段蒸散发量ED（mm）ED=IF(AE3>=(I4*(D4-T4)),0,I4*(D4-T4)-U4)
    // ED=IF(WL0>=(C*(EM-EU)),0,C*(EM-EU)-EL)
    if (preWL >= this->C * (EM - EU)) {
        ED = 0.0;
    } else {
        ED = this->C * (EM - EU) - EL;
    }
    double E;// 流域总蒸散发量E（mm）E=V4+T4+U4
    // E=ED+EU+EL
    E = ED + EU + EL;

    double PE; // 净雨PE（mm）PE=B4-W4
    // PE=P-E
    PE = P - E;

    double WM; // 流域平均蓄水容量WM（mm）WM=E4+F4+G4
    // WM=WUM+WLM+WDM
    WM = this->WUM + this->WLM + this->WDM;
    double WMM; // 流域上最大蓄水容量WMM（mm）=Z4*(1+J4)
    // WMM（mm）=WM*(1+B)
    WMM = WM * (1 + this->B);
    double A; // 与流域蓄水量相对应的纵坐标值A=AA4*(1-POWER((1-Y3/Z4),(1/(1+J4))))
    // A=WMM*(1-POWER((1-W0/WM),(1/(1+B))))
    double preW = preWU + preWL + preWD;
    A = WMM * (1 - pow(fabs(1 - preW / WM), (1 / (1 + this->B))));

    double R; // 总径流量R（mm）=IF(X4>0,IF(X4+AB4>=AA4,X4-Z4+Y3,X4-((Z4-Y3)-Z4*POWER((1-(X4+AB4)/AA4),(1+J4)))),0)
    // R（mm）=IF(PE>0,IF(PE+A>=WMM,PE-WM+W0,PE-((WM-W0)-WM*POWER((1-(PE+A)/WMM),(1+B)))),0)
    if (PE > 0) {
        if (PE + A >= WMM) {
            R = PE - WM + preW;
        } else {
            R = PE - ((WM - preW) - WM * pow((1 - (PE + A) / WMM), (1 + this->B)));
        }
    } else {
        R = 0.0;
    }

    // 上层土壤蓄水容量WU（mm）=IF(X4>0,IF((AD3+X4-AC4)<E4,(AD3+X4-AC4),E4),IF((AD3-ABS(X4))<0,0,(AD3-ABS(X4))))
    // WU（mm）=IF(PE>0,IF((WU0+PE-R)<WUM,(WU0+PE-R),WUM),IF((WU0-ABS(PE))<0,0,(WU0-ABS(PE))))
    if (PE > 0) {
        if ((preWU + PE - R) < this->WUM) {
            nowWU = (preWU + PE - R);
        } else {
            nowWU = this->WUM;
        }
    } else {
        if ((preWU - fabs(PE)) < 0) {
            nowWU = 0.0;
        } else {
            nowWU = (preWU - fabs(PE));
        }
    }
    // 下层土壤蓄水容量WL（mm）
    // =IF(X4>=0,IF(AD3>=E4,IF(AE3+X4-AC4>=F4,F4,(AE3+X4-AC4)),IF(X4-AC4>E4-AD3,IF(AE3+X4-AC4-E4+AD3>=F4,F4,AE3+X4-AC4-E4+AD3),AE3)),IF(AD4-ABS(X4)<0,IF(AE3-ABS(AD4-ABS(X4))<0,0,AE3-ABS(AD4-ABS(X4))),AE3))
    // IF(PE>=0,IF(WU0>=WUM,IF(WL0+PE-R>=WLM,WLM,(WL0+PE-R)),IF(PE-R>WUM-WU0,IF(WL0+PE-R-WUM+WU0>=WLM,WLM,WL0+PE-R-WUM+WU0),WL0)),IF(WU-ABS(PE)<0,IF(WL0-ABS(WU-ABS(PE))<0,0,WL0-ABS(WU-ABS(PE))),WL0))
    if (PE >= 0) {
        if (preWU >= this->WUM) {
            if (preWL + PE - R >= this->WLM) {
                nowWL = this->WLM;
            } else {
                nowWL = (preWL + PE - R);
            }
        } else {
            if (PE - R > this->WUM - preWU) {
                if (preWL + PE - R - this->WUM + preWU >= this->WLM) {
                    nowWL = this->WLM;
                } else {
                    nowWL = preWL + PE - R - this->WUM + preWU;
                }
            } else {
                nowWL = preWL;
            }
        }
    } else {
        if (nowWU - fabs(PE) < 0) {
            if (preWL - fabs(nowWU - fabs(PE)) < 0) {
                nowWL = 0.0;
            } else {
                nowWL = preWL - fabs(nowWU - fabs(PE));
            }
        } else {
            nowWL = preWL;
        }
    }
    // 深层土壤蓄水容量WD（mm）==IF(X4>=0,IF(AE3>=F4,IF(AF3+X4-AC4>=G4,G4,(AF3+X4-AC4)),IF(X4-AC4>E4-AD3+F4-AE3,IF(AF3+X4-AC4-E4+AD3-F4+AE3>=G4,G4,AF3+X4-AC4-E4+AD3-F4+AE3),AF3)),IF(AE4-ABS(X4)<0,IF(AF3-ABS(AE4-ABS(X4))<0,0,AF3-ABS(AE4-ABS(X4))),AF3))
    //WD（mm）==IF(PE>=0,IF(WL0>=WLM,IF(WD0+PE-R>=WDM,WDM,(WD0+PE-R)),IF(PE-R>WUM-WU0+WLM-WL0,IF(WD0+PE-R-WUM+WU0-WLM+WL0>=WDM,WDM,WD0+PE-R-WUM+WU0-WLM+WL0),WD0)),IF(WL-ABS(PE)<0,IF(WD0-ABS(WL-ABS(PE))<0,0,WD0-ABS(WL-ABS(PE))),WD0))
    if (PE >= 0) {
        if (preWL >= this->WLM) {
            if (preWD + PE - R >= this->WDM) {
                nowWD = this->WDM;
            } else {
                nowWD = (preWD + PE - R);
            }
        } else {
            if (PE - R > this->WUM - preWU + this->WLM - preWL) {
                if (preWD + PE - R - this->WUM + preWU - this->WLM + preWL >= this->WDM) {
                    nowWD = this->WDM;
                } else {
                    nowWD = preWD + PE - R - this->WUM + preWU - this->WLM + preWL;
                }
            } else {
                nowWD = preWD;
            }
        }
    } else {
        if (nowWL - fabs(PE) < 0) {
            if (preWD - fabs(nowWL - fabs(PE)) < 0) {
                nowWD = 0.0;
            } else {
                nowWD = preWD - fabs(nowWL - fabs(PE));
            }
        } else {
            nowWD = preWD;
        }
    }
    double nowW; // 流域蓄水量W（mm）W=AD4+AE4+AF4
    // W=WU+WL+WD
    nowW = nowWU + nowWL + nowWD;


    // 2. 自由水蓄水库

    // 产流面积FR=IF(X4<=0,0,AC4/X4)
    // FR=IF(PE<=0,0,R/PE)
    if (PE <= 0) {
        nowFR = 0.0;
    } else {
        nowFR = R / PE;
    }

    double MS; // 流域上单点最大自由蓄水量MS（mm）=(1+M4)*L4
    // MS（mm）=(1+EX)*SM
    MS = (1 + this->EX) * this->SM;

    double AU; // 与自由蓄水量S相对应的纵坐标值AU=AG4*(1-POWER((1-AK3/L4),(1/(1+M4))))
    // =IF(AI4>0,IF(AG4*(1-POWER((1-AK3*(AI3/AI4)/L4),(1/(1+M4))))>0,AG4*(1-POWER((1-AK3*(AI3/AI4)/L4),(1/(1+M4)))),0),0)
    // AU=MS*(1-POWER((1-S0/SM),(1/(1+EX))))
//    AU = MS * (1 - pow(fabs(1 - preS / this->SM), (1 / (1 + this->EX))));
    if (nowFR > 0) {
        if (MS * (1 - pow(1 - preS * (preFR / nowFR) / this->SM, (1 / (1 + this->EX)))) > 0) {
            AU = MS * (1 - pow(1 - preS * (preFR / nowFR) / this->SM, (1 / (1 + this->EX))));
        } else {
            AU = 0;
        }
    } else {
        AU = 0;
    }

    double RS; // 地表径流量RS（mm)
    // =IF(AI4<=0,0,IF((X4+AH4)<AG4,AI4*(X4+AK3*AI3/AI4-L4+L4*POWER((1-(X4+AH4)/AG4),(M4+1))),AI4*(X4+AK3*AI3/AI4-L4)))
    if (nowFR <= 0) {
        RS = 0;
    } else {
        if ((PE + AU) < MS) {
            RS = nowFR *
                 (PE + preS * preFR / nowFR - this->SM + this->SM * pow((1 - (PE + AU) / MS), (this->EX + 1)));
        } else {
            RS = nowFR * (PE + preS * preFR / nowFR - this->SM);
        }
    }

    // 除地面径流后自由蓄水量SR
    // =IF(AI4>0,IF(AK3<=L4,(AK3*AI3/AI4+(AC4-AL4)/AI4),20),AK3)
    if (nowFR > 0) {
        if (preS <= this->SM) {
            nowSR = (preS * preFR / nowFR + (R - RS) / nowFR);
        } else {
            nowSR = this->WUM; // todo why is const 20, I change it to WUM
        }
    } else {
        nowSR = preS;
    }

    // 流域自由蓄水量S（mm）=AJ4*(1-P4-O4)
    nowS = nowSR * (1 - this->KI - this->KSS);

    double RI; // 壤中流量RI（mm）=O4*AJ4*AI4
    // RI（mm）=KSS*SR*FR
    RI = this->KSS * nowSR * nowFR;
    double RG; // 地下径流量RG（mm）=N4*AJ4*AI4
    // RG（mm）=KG*SR*FR
    RG = this->KG * nowSR * nowFR;


    double amendRS; // 地表产流修正RS1 =IF(X4>0,AL4*AC4/(AL4+AM4+AN4),0)
    // RS1 =IF(PE>0,RS*R/(RS+RI+RG),0)
    if (PE > 0) {
        amendRS = RS * R / (RS + RI + RG);
    } else {
        amendRS = 0.0;
    }
    double amendRI; // 壤中流产流修正RI1 =IF(X4>0,AM4*AC4/(AL4+AM4+AN4),0)
    // RI1 =IF(PE>0,RI*R/(RS+RI+RG),0)
    if (PE > 0) {
        amendRI = RI * R / (RS + RI + RG);
    } else {
        amendRI = 0.0;
    }
    double amendRG; // 地下径流修正RG1 =IF(X4>0,AN4*AC4/(AL4+AM4+AN4),0)
    // RG1 =IF(PE>0,RG*R/(RS+RI+RG),0)
    if (PE > 0) {
        amendRG = RG * R / (RS + RI + RG);
    } else {
        amendRG = 0.0;
    }

    // 不透水面积产流Rim
    double Rim = 0.0;
    if (PE > 0) {
        Rim = PE * this->IMP; // 与RS直接相加
    }
    amendRS += Rim;

    // 3.汇流计算
    // 地表径流流量QRS（m3/s）=AR3*POWER(P4,S4/24)+AO4*(1-POWER(P4,S4/24))*(537/3.6/S4)
    // QRS（m3/s）=QRS0*POWER(KI,S4/24)+RS1*(1-POWER(KI,S4/24))*(537/3.6/S4)
    nowQRS = preQRS * pow(this->KI, configValues.time_stride / 24) +
             amendRS * (1 - pow(this->KI, configValues.time_stride / 24)) *
             (pModelContext.area / 3.6 / configValues.time_stride);
    // 壤中流流量QRSS（m3/s）=AS3*POWER(R4,S4/24)+AP4*(1-POWER(R4,S4/24))*(537/3.6/S4)
    // QRSS（m3/s）=QRSS0*POWER(KKSS,S4/24)+RI1*(1-POWER(KKSS,S4/24))*(537/3.6/S4)
    nowQRSS = preQRSS * pow(this->KKSS, configValues.time_stride / 24) +
              amendRI * (1 - pow(this->KKSS, configValues.time_stride / 24)) *
              (pModelContext.area / 3.6 / configValues.time_stride);
    // 地下径流流量QRG（m3/s）=AT3*POWER(Q4,S4/24)+AQ4*(1-POWER(Q4,S4/24))*(537/3.6/S4)
    // QRG（m3/s）=QRG0*POWER(KKG,S4/24)+RG1*(1-POWER(KKG,S4/24))*(537/3.6/S4)
    nowQRG = preQRG * pow(this->KKG, configValues.time_stride / 24) +
             amendRG * (1 - pow(this->KKG, configValues.time_stride / 24)) *
             (pModelContext.area / 3.6 / configValues.time_stride);
    // 总径流流量Q（m3/s）=AR4+AS4+AT4
    // Q（m3/s）=AR4+AS4+AT4
    pModelContext.res_flow = nowQRS + nowQRSS + nowQRG;
    return RoutingDataMeta(nowQRS + nowQRSS + nowQRG);
}

std::vector<std::string> XAJRunoffModel::getParaNames(const ModelContext *pModelContext) {
    return {"WUM", "WLM", "WDM", "K", "C", "B", "IMP", "SM",
                     "EX", "KG", "KSS", "KI", "KKG", "KKSS"};
}

std::vector<std::string> XAJRunoffModel::getInitNames(const ModelContext *pModelContext) {
    return {"WU0", "WL0", "WD0", "FR0", "S0", "QRS0", "QRSS0", "QRG0"};
}

std::vector<std::string> XAJRunoffModel::getInputNames(const ModelContext *pModelContext) {
    return {"P", "EI"};
}
