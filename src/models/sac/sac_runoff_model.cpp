//
// Created by wujiahao on 18-7-31.
//

#include <cmath>
#include <iostream>
#include <fstream>
#include <map>
#include "sac_runoff_model.h"
#include "../model_warehouse.h"

const std::string &SACRunoffModel::MODEL_NAME = models::SAC_MODEL_NAME;

SACRunoffModel::SACRunoffModel(ModelContext *pModelContext) :
        BaseModel(pModelContext),
        KC(pModelContext->getParamData("KC")),
        UZTWM(pModelContext->getParamData("UZTWM")),
        LZTWM(pModelContext->getParamData("LZTWM")),
        SARVA(pModelContext->getParamData("SARVA")),
        PCTIM(pModelContext->getParamData("PCTIM")),
        UZK(pModelContext->getParamData("UZK")),
        ADIMP(pModelContext->getParamData("ADIMP")),
        LZSK(pModelContext->getParamData("LZSK")),
        LZPK(pModelContext->getParamData("LZPK")),
        LZFPM(pModelContext->getParamData("LZFPM")),
        LZFSM(pModelContext->getParamData("LZFSM")),
        ZPERC(pModelContext->getParamData("ZPERC")),
        REXP(pModelContext->getParamData("REXP")),
        UZFWM(pModelContext->getParamData("UZFWM")),
        PFREE(pModelContext->getParamData("PFREE")),
        RSERV(pModelContext->getParamData("RSERV")),
        CS(pModelContext->getParamData("CS")),
        CI(pModelContext->getParamData("CI")),
        CG(pModelContext->getParamData("CG")),
        //状态
        UZFWC0(pModelContext->getInitData("UZFWC0")),
        UZTWC0(pModelContext->getInitData("UZTWC0")),
        LZTWC0(pModelContext->getInitData("LZTWC0")),
        LZFSC0(pModelContext->getInitData("LZFSC0")),
        LZFPC0(pModelContext->getInitData("LZFPC0")),
        QRS0(pModelContext->getInitData("QRS0")),    // 地下水初始流量（m3/s）
        QRSS0(pModelContext->getInitData("QRSS0")),  // 壤中流初始流量（m3/s）
        QRG0(pModelContext->getInitData("QRG0")),    // 地下水初始流量（m3/s）

        // input data
        Ps(pModelContext->getInputData("P")),
        EIs(pModelContext->getInputData("EI")) {}

RoutingDataMeta
SACRunoffModel::runModel(ModelContext &pModelContext, const Config &configValues, const RoutingDataMeta &upRoutDatas,
                         int nowTimeStep) {
    double EI = EIs[nowTimeStep];
    double P = Ps[nowTimeStep];

    // P=B4 EI=C4 EP=D4
    // PCTIM=E4 ADIMP=F4 SARVA=G4 UZTWM=H4 LZTWM=I4 RSERV=J4 UZK=K4 UZFWM=L4 ZPERC=M4 REXP=N4 PFREE=O4 LZFSM=P4 LZSK=Q4 LZFPM=R4 LZPK=S4 DT=T4
    // UZFWC=U4 UZTWC=V4 LZTWC=W4 LZFSC=X4 LZFPC=Y4 E1=Z4 E2=AA4 E3=AB4 E4=AC4 E5=AD4 E=AE4 ROIMP=AF4 PAV=AG4 ADDRO=AH4 ADSUR=AI4 RI=AJ4 RGs=AK4
    // RGp=AL4 PBASE=AM4 DEFR=AN4 PERC=AO4 ADDLZFW=AP4 ADDLZTW=AQ4
    // PERCP=AR4 PERCS=AS4 CHECK=AT4 UZFWC1=AU4 UZTWC1=AV4 LZTWC1=AW4 LZFSC1=AX4 LZFPC1=AY4 UZFWC2=AZ4 UZTWC2=BA4
    // SAVED=BB4 DEL=BC4 LZTWC2=BD4 LZFSC2=BE4 P=BF4 E=BG4 R=BH4 DW=BI4 CKERR=BJ4 RS=BK4 RI=BL4 RG=BM4 CS=BN4 CG=BO4 CI=BP4 QRS=BQ4 QRSS=BR4 QRG=BS4 Q=BT4

    double preUZFWC, preUZTWC, preLZTWC, preLZFSC, preLZFPC, preQRS, preQRSS, preQRG;
    if (nowTimeStep == 0) {
        preUZFWC = UZFWC0;
        preUZTWC = UZTWC0;
        preLZTWC = LZTWC0;
        preLZFSC = LZFSC0;
        preLZFPC = LZFPC0;
        preQRS = QRS0;
        preQRSS = QRSS0;
        preQRG = QRG0;
    } else {
        preUZFWC = UZFWC;
        preUZTWC = UZTWC;
        preLZTWC = LZTWC;
        preLZFSC = LZFSC;
        preLZFPC = LZFPC;
        preQRS = QRS;
        preQRSS = QRSS;
        preQRG = QRG;
    }

    // 蒸散发计算
    double EP = EI * KC;  // 蒸发能力EP(mm)
    double E1; // 上土层张力水蒸散发量E1=IF(V3>=D4,D4*V3/H4,V3)
    // E1=IF(preUZTWC>=EP,EP*preUZTWC/UZTWM,preUZTWC)
    if (preUZTWC >= EP) {
        E1 = EP * preUZTWC / UZTWM;
    } else {
        E1 = preUZTWC;
    }
    double E2; // 上土层自由水蒸散发量E2=IF(D4-Z4=0,0,IF(U3>=(D4-Z4),D4-Z4,U3))
    // E2=IF(EP-E1=0,0,IF(preUZFWC>=(EP-E1),EP-E1,preUZFWC))
    if (EP - E1 == 0) {
        E2 = 0;
    } else {
        if (preUZFWC >= (EP - E1)) {
            E2 = EP - E1;
        } else {
            E2 = preUZFWC;
        }
    }
    double E3; //   下土层张力水蒸散发量E3=(D4-Z4-AA4)*W3/(H4+I4)
    // E3=(EP-E1-E2)*preLZTWC/(UZTWM+LZTWM)
    E3 = (EP - E1 - E2) * preLZTWC / (UZTWM + LZTWM);
    double E4; //   水面蒸发量E4=IF(G4<=E4,D4*G4,D4*G4-(Z4+AA4+AB4)*(G4-E4))
    // E4=IF(SARVA<=PCTIM,EP*SARVA,EP*SARVA-(E1+E2+E3)*(SARVA-PCTIM))
    if (SARVA <= PCTIM) {
        E4 = EP * SARVA;
    } else {
        E4 = EP * SARVA - (E1 + E2 + E3) * (SARVA - PCTIM);
    }
    double E5; //   可变不透水面积上的蒸散发量E5=Z4+(D4-Z4)*(V3+W3-V3)/(H4+I4)
    // E5=E1+(EP-E1)*(preUZTWC+preLZTWC-preUZTWC)/(UZTWM+LZTWM)
    E5 = E1 + (EP - E1) * (preUZTWC + preLZTWC - preUZTWC) / (UZTWM + LZTWM);
    double E;//   总蒸发量E=SUM(Z4:AD4)
    // E=SUM(E1:E5)
    E = E1 + E2 + E3 + E4 + E5;


    // 产流量计算
    double ROIMP; //   永久不透水直接径流ROIMP=B4*E4
    // ROIMP=P*PCTIM
    ROIMP = P * PCTIM;
    double PAV; //   有效降雨PAV=IF(B4<=(H4-V3),0,(B4+V3)-H4)
    // PAV=IF(P<=(UZTWM-preUZTWC),0,(P+preUZTWC)-UZTWM)
    if (P <= (UZTWM - preUZTWC)) {
        PAV = 0;
    } else {
        PAV = (P + preUZTWC) - UZTWM;
    }
    double ADDRO; //   可变不透水面积直接径流ADDRO=AG4*((V3+W3-V3)/I4)
    // ADDRO=PAV*((preUZTWC+preLZTWC-preUZTWC)/LZTWM)
    ADDRO = PAV * ((preUZTWC + preLZTWC - preUZTWC) / LZTWM);
    double ADSUR; //   地面径流ADSUR=AG4*(1-E4-F4)
    // ADSUR=PAV*(1-PCTIM-ADIMP)
    ADSUR = PAV * (1 - PCTIM - ADIMP);
    double RI; //   壤中流RI=U3*(1-(1-K4)^(T4/24))*(1-E4-F4)
    // RI=preUZFWC*(1-(1-UZK)^(DT/24))*(1-PCTIM-ADIMP)
    RI = preUZFWC * (1 - pow((1 - UZK), (configValues.time_stride / 24))) * (1 - PCTIM - ADIMP);
    double RGs; //   快速地下水RGs=X3*(1-(1-Q4)^(T4/24))*(1-E4-F4)
    // RGs=preLZFSC*(1-(1-LZSK)^(DT/24))*(1-PCTIM-ADIMP)
    RGs = preLZFSC * (1 - pow((1 - LZSK), (configValues.time_stride / 24))) * (1 - PCTIM - ADIMP);
    double RGp; //   慢速地下水RGp=Y3*(1-(1-S4)^(T4/24))*(1-E4-F4)
    // RGp=preLZFPC*(1-(1-LZPK)^(DT/24))*(1-PCTIM-ADIMP)
    RGp = preLZFPC * (1 - pow((1 - LZPK), (configValues.time_stride / 24))) * (1 - PCTIM - ADIMP);


    // 下渗量计算
    double PBASE; //   稳定下渗量PBASE=R4*S4+P4*Q4
    // PBASE=LZFPM*LZPK+LZFSM*LZSK
    PBASE = LZFPM * LZPK + LZFSM * LZSK;
    double DEFR; //   下土层缺水率DEFR=1-(Y3+X3+W3)/(R4+P4+I4)
    // DEFR=1-(preLZFPC+preLZFSC+preLZTWC)/(LZFPM+LZFSM+LZTWM)
    DEFR = 1 - (preLZFPC + preLZFSC + preLZTWC) / (LZFPM + LZFSM + LZTWM);
    double PERC; //   实际下渗量PERC	=AM4*(1+M4+AN4^N4)*U3/L4
    // PERC	=PBASE*(1+ZPERC+DEFR^REXP)*preUZFWC/UZFWM
    PERC = PBASE * (1 + ZPERC + pow(DEFR, REXP)) * preUZFWC / UZFWM;
    double ADDLZFW; //   下渗下土层自由水ADDLZFW=AO4*O4
    // ADDLZFW=PERC*PFREE
    ADDLZFW = PERC * PFREE;
    double ADDLZTW; //   下渗下土层张力水ADDLZTW=AO4*(1-O4)
    // ADDLZTW=PERC*(1-PFREE)
    ADDLZTW = PERC * (1 - PFREE);
    double PERCP; //   下渗慢速自由水水量PERCP=AP4*(R4/(R4+P4))*(2*(1-Y3/R4)/((1-Y3/R4)+(1-X3/P4)))
    // PERCP=ADDLZFW*(LZFPM/(LZFPM+LZFSM))*(2*(1-preLZFPC/LZFPM)/((1-preLZFPC/LZFPM)+(1-preLZFSC/LZFSM)))
    PERCP = ADDLZFW * (LZFPM / (LZFPM + LZFSM)) *
            (2 * (1 - preLZFPC / LZFPM) / ((1 - preLZFPC / LZFPM) + (1 - preLZFSC / LZFSM)));
    double PERCS; //   下渗快速自由水水量PERCS=AP4-AR4
    // PERCS=ADDLZFW-PERCP
    PERCS = ADDLZFW - PERCP;
    double CHECK; //   上土层自由水蓄量增加量CHECK=IF(AO4>(I4+P4+R4-W3-X3-Y3),(AO4+Y3+X3+W3)-(I4+P4+R4),0)
    // CHECK=IF(PERC>(LZTWM+LZFSM+LZFPM-preLZTWC-preLZFSC-preLZFPC),(PERC+preLZFPC+preLZFSC+preLZTWC)-(LZTWM+LZFSM+LZFPM),0)
    if (PERC > (LZTWM + LZFSM + LZFPM - preLZTWC - preLZFSC - preLZFPC)) {
        CHECK = (PERC + preLZFPC + preLZFSC + preLZTWC) - (LZTWM + LZFSM + LZFPM);
    } else {
        CHECK = 0;
    }


    // 时段土层蓄量校核
    double UZFWC1; //    时段末上土层自由水蓄量UZFWC1=U3-AA4-AJ4+AT4
    // UZFWC1=preUZFWC-E2-RI+CHECK
    UZFWC1 = preUZFWC - E2 - RI + CHECK;
    double UZTWC1; //    时段末上土层张力水蓄量UZTWC1=V3-Z4
    // UZTWC1=preUZTWC-E1
    UZTWC1 = preUZTWC - E1;
    double LZTWC1; //    时段末下土层张力水蓄量LZTWC1=W3-AB4
    // LZTWC1=preLZTWC-E3
    LZTWC1 = preLZTWC - E3;
    double LZFSC1; //    时段末快速地下水蓄量LZFSC1=X3-AK4
    // LZFSC1=preLZFSC-RGs
    LZFSC1 = preLZFSC - RGs;
    double LZFPC1; //    时段末慢速地下水蓄量LZFPC1=Y3-AL4
    // LZFPC1=preLZFPC-RGp
    LZFPC1 = preLZFPC - RGp;

    // 水量平衡校核
    double R; //    时段产流R	=AF4+AH4+AI4+AJ4+AK4+AL4
    // R	=ROIMP+ADDRO+ADSUR+RI+RGs+RGp
    R = ROIMP + ADDRO + ADSUR + RI + RGs + RGp;
    double DW; //    时段蓄量变化DW=(AU4+AV4+AW4+AX4+AY4)-(U3+V3+W3+X3+Y3)
    // DW=(UZFWC1+UZTWC1+LZTWC1+LZFSC1+LZFPC1)-(preUZFWC+preUZTWC+preLZTWC+preLZFSC+preLZFPC)
    DW = (UZFWC1 + UZTWC1 + LZTWC1 + LZFSC1 + LZFPC1) - (preUZFWC + preUZTWC + preLZTWC + preLZFSC + preLZFPC);
    double CKERR; //    校核误差CKERR=BF4-BG4-BH4-BI4
    // CKERR=P-E-R-DW
    CKERR = P - E - R - DW;

    double UZFWC2; //    校核时段末上土层自由水蓄量UZFWC2=IF((AU4/L4)>(AV4/H4),L4*(AV4+AU4)/(H4+L4)+BJ4,AU4+BJ4)
    // UZFWC2=IF((UZFWC1/UZFWM)>(UZTWC1/UZTWM),UZFWM*(UZTWC1+UZFWC1)/(UZTWM+UZFWM)+CKERR,UZFWC1+CKERR)
    if ((UZFWC1 / UZFWM) > (UZTWC1 / UZTWM)) {
        UZFWC2 = UZFWM * (UZTWC1 + UZFWC1) / (UZTWM + UZFWM) + CKERR;
    } else {
        UZFWC2 = UZFWC1 + CKERR;
    }
    double UZTWC2; //    校核时段末上土层张力水蓄量UZTWC2=IF((AU4/L4)>(AV4/H4),H4*(AU4+AV4)/(H4+L4),AV4)
    // UZTWC2=IF((UZFWC1/UZFWM)>(UZTWC1/UZTWM),UZTWM*(UZFWC1+UZTWC1)/(UZTWM+UZFWM),UZTWC1)
    if ((UZFWC1 / UZFWM) > (UZTWC1 / UZTWM)) {
        UZTWC2 = UZTWM * (UZFWC1 + UZTWC1) / (UZTWM + UZFWM);
    } else {
        UZTWC2 = UZTWC1;
    }
    double SAVED; //    不参与蒸散发的自由水蓄量SAVED=J4*(P4+R4)
    // SAVED=RSERV*(LZFSM+LZFPM)
    SAVED = RSERV * (LZFSM + LZFPM);
    double DEL; //    下土层自由水补充张力水量DEL=IF((AW4/I4)<((AY4+AX4-BB4+AW4)/(R4+P4-BB4+I4)),I4*((AY4+AX4+AW4-BB4)/(R4+P4+I4-BB4)-AW4/I4),0)
    // DEL=IF((LZTWC1/LZTWM)<((LZFPC1+LZFSC1-SAVED+LZTWC1)/(LZFPM+LZFSM-SAVED+LZTWM)),LZTWM*((LZFPC1+LZFSC1+LZTWC1-SAVED)/(LZFPM+LZFSM+LZTWM-SAVED)-LZTWC1/LZTWM),0)
    if ((LZTWC1 / LZTWM) <
        ((LZFPC1 + LZFSC1 - SAVED + LZTWC1) / (LZFPM + LZFSM - SAVED + LZTWM))) {
        DEL = LZTWM *
              ((LZFPC1 + LZFSC1 + LZTWC1 - SAVED) / (LZFPM + LZFSM + LZTWM - SAVED) -
               LZTWC1 / LZTWM);
    } else {
        DEL = 0;
    }
    double LZTWC2;//    校核时段末下土层张力水蓄量LZTWC2=AW4+BC4
    // LZTWC2=LZTWC1+DEL
    LZTWC2 = LZTWC1 + DEL;
    double LZFSC2; //    校核时段末快速地下水蓄量LZFSC2=AX4-BC4
    // LZFSC2=LZFSC1-DEL
    LZFSC2 = LZFSC1 - DEL;

    // 土层蓄量
    //   上土层自由水蓄量UZFWC=IF(AZ4<=0,0,IF(AZ4<L4,AZ4,L4))
    // UZFWC=IF(UZFWC2<=0,0,IF(UZFWC2<UZFWM,UZFWC2,UZFWM))
    if (UZFWC2 <= 0) {
        UZFWC = 0;
    } else {
        if (UZFWC2 < UZFWM) {
            UZFWC = UZFWC2;
        } else {
            UZFWC = UZFWM;
        }
    }
    //   上土层张力水蓄量UZTWC=IF(BA4<H4,BA4,H4)
    // UZTWC=IF(UZTWC2<UZTWM,UZTWC2,UZTWM)
    if (UZTWC2 < UZTWM) {
        UZTWC = UZTWC2;
    } else {
        UZTWC = UZTWM;
    }
    //   下土层张力水蓄量LZTWC=IF(BD4<I4,BD4,I4)
    // LZTWC=IF(LZTWC2<LZTWM,LZTWC2,LZTWM)
    if (LZTWC2 < LZTWM) {
        LZTWC = LZTWC2;
    } else {
        LZTWC = LZTWM;
    }
    //   快速地下水蓄量LZFSC=IF(BE4<P4,BE4,P4)
    // LZFSC=IF(LZFSC2<LZFSM,LZFSC2,LZFSM)
    if (LZFSC2 < LZFSM) {
        LZFSC = LZFSC2;
    } else {
        LZFSC = LZFSM;
    }
    //   慢速地下水蓄量LZFPC=IF(AY4<R4,AY4,R4)
    // LZFPC=IF(LZFPC1<LZFPM,LZFPC1,LZFPM)
    if (LZFPC1 < LZFPM) {
        LZFPC = LZFPC1;
    } else {
        LZFPC = LZFPM;
    }


    // 坡面汇流计算									
    double RS; //    地面径流RS=AF4+AH4+AI4
    // RS=ROIMP+ADDRO+ADSUR
    RS = ROIMP + ADDRO + ADSUR;
    double RG; //    地下径流RG=AK4+AL4
    // RG=RGs+RGp
    RG = RGs + RGp;
    //    地表径流流量QRS（m3/s）=BQ3*POWER(BN4,T4/24)+BK4*(1-POWER(BN4,T4/24))*(537/3.6/T4)
    // QRS（m3/s）=BQ3*POWER(CS,DT/24)+RS*(1-POWER(CS,DT/24))*(537/3.6/DT)
    QRS = preQRS * pow(CS, configValues.time_stride / 24) +
          RS * (1 - pow(CS, configValues.time_stride / 24)) * (pModelContext.area / 3.6 / configValues.time_stride);
    //    壤中流流量QRSS（m3/s）=BR3*POWER(BP4,T4/24)+BL4*(1-POWER(BP4,T4/24))*(537/3.6/T4)
    // QRSS（m3/s）=BR3*POWER(CI,DT/24)+RI*(1-POWER(CI,DT/24))*(537/3.6/DT)
    QRSS = preQRSS * pow(CI, configValues.time_stride / 24) +
           RI * (1 - pow(CI, configValues.time_stride / 24)) * (pModelContext.area / 3.6 / configValues.time_stride);
    //    地下径流流量QRG（m3/s）=BS3*POWER(BO4,T4/24)+BM4*(1-POWER(BO4,T4/24))*(537/3.6/T4)
    // QRG（m3/s）=BS3*POWER(CG,DT/24)+RG*(1-POWER(CG,DT/24))*(537/3.6/DT)
    QRG = preQRG * pow(CG, configValues.time_stride / 24) +
          RG * (1 - pow(CG, configValues.time_stride / 24)) * (pModelContext.area / 3.6 / configValues.time_stride);
    //    总径流流量Q（m3/s)=BQ4+BR4+BS4
    // Q（m3/s)=QRS+QRSS+QRG
    pModelContext.res_flow = QRS + QRSS + QRG;
    return RoutingDataMeta(pModelContext.res_flow);
}

std::vector<std::string> SACRunoffModel::getParaNames(const ModelContext *pModelContext) {
    return {"KC", "UZTWM", "LZTWM", "SARVA",
            "PCTIM", "UZK", "ADIMP", "LZSK",
            "LZPK", "LZFPM", "LZFSM", "ZPERC",
            "REXP", "UZFWM", "PFREE", "RSERV",
            "CS", "CI", "CG"};
}

std::vector<std::string> SACRunoffModel::getInitNames(const ModelContext *pModelContext) {
    return {"UZFWC0", "UZTWC0", "LZTWC0", "LZFSC0", "LZFPC0", "QRS0", "QRSS0", "QRG0"};
}

std::vector<std::string> SACRunoffModel::getInputNames(const ModelContext *pModelContext) {
    return {"P", "EI"};
}