/*
  Shugong Wang shw36@pitt.edu
  University of Pittsburgh
*/

#include "math.h"
#include "stdio.h"
#include "XinanjiangModel.h"

//#include "conio.h"
XinanjiangModel::XinanjiangModel(void) {
    this->E = NULL;
    this->P = NULL;

    this->E = NULL;
    this->ED = NULL;
    this->EL = NULL;
    this->EU = NULL;

    this->QRG = NULL;
    this->QRS = NULL;

    this->R = NULL;
    this->RG = NULL;
    this->RS = NULL;
}

XinanjiangModel::~XinanjiangModel(void) {
    delete[] this->P;
    delete[] this->EI;

    delete[] this->E;
    delete[] this->ED;
    delete[] this->EL;
    delete[] this->EU;

    delete[] this->QRG;
    delete[] this->QRS;
    delete[] this->Q;

    delete[] this->R;
    delete[] this->RG;
    delete[] this->RS;

    delete[] this->W;
    delete[] this->WD;
    delete[] this->WL;
    delete[] this->WU;
}

// 通过文件初始化模型设置，包括步长，步数，驱动数据
XinanjiangModel::XinanjiangModel(char *FileName) {
}

// 初始化模型
void XinanjiangModel::InitModel(long nSteps, double Area, int DeltaT, char *ForcingFile) {
    FILE *fp;
    int i;
    this->m_nSteps = nSteps;
    // 驱动数据
    this->P = new double[this->m_nSteps];
    this->EI = new double[this->m_nSteps];
    // 模型输出，蒸散发项
    this->E = new double[this->m_nSteps];
    this->ED = new double[this->m_nSteps];
    this->EL = new double[this->m_nSteps];
    this->EU = new double[this->m_nSteps];
    // 模型输出，出流项，经过汇流的产流
    this->QRG = new double[this->m_nSteps];
    this->QRS = new double[this->m_nSteps];
    this->Q = new double[this->m_nSteps];
    // 模型输出，产流项
    this->R = new double[this->m_nSteps];
    this->RG = new double[this->m_nSteps];
    this->RS = new double[this->m_nSteps];
    // 模型状态量，土壤湿度
    this->W = new double[this->m_nSteps];
    this->WD = new double[this->m_nSteps];
    this->WL = new double[this->m_nSteps];
    this->WU = new double[this->m_nSteps];

    this->m_Area = Area;
    this->m_DeltaT = DeltaT;
    this->m_U = 1.0;//this->m_Area/(3.6*this->m_DeltaT);
    // Forcing文件的格式：第一列：降水（单位毫米）空格第二列水面蒸发（毫米）
    if ((fp = fopen(ForcingFile, "r")) == NULL) {
        printf("Can not open forcing file!\n");
        return;
    }
    for (i = 0; i < this->m_nSteps; i++) {
        fscanf(fp, "%lf%lf", &(this->P[i]), &(this->EI[i]));
    }
    fclose(fp);
}

// 设置模型参数
void XinanjiangModel::SetParameters(double *Params) {
    this->K = Params[0];     // (1) 流域蒸散发能力与实测水面蒸发之比
    this->IMP = Params[1];     // (2) 流域不透水面积占全流域面积之比
    this->B = Params[2];     // (3) 蓄水容量曲线的方次
    this->WUM = Params[3];     // (4) 上层蓄水容量
    this->WLM = Params[4];     // (5) 下层蓄水容量
    this->WDM = Params[5];     // (6) 深层蓄水容量
    this->C = Params[6];     // (7) 深层蒸散发系数
    this->FC = Params[7];     // (8) 稳定入渗率（毫米／小时）
    this->KKG = Params[8];     // (9) 地下径流消退系数
    this->KSTOR = Params[9];     // (10)汇流计算参数

    this->WM = this->WUM + this->WLM + this->WDM;
    this->WMM = this->WM * (1.0 + this->B) / (1.0 - this->IMP);

}

// 运行新安江模型
void XinanjiangModel::RunModel(void) {
    long i;
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

    // 假设流域经历了长时间降水，各层土壤含水量均为该层土壤的蓄水能力
    double W = this->WM;   // 流域内土壤湿度
    double WU = this->WUM;  // 流域内上层土壤湿度
    double WL = this->WLM;  // 流域内下层土壤适度
    double WD = this->WDM;  // 流域内深层土壤湿度

    for (i = 0; i < this->m_nSteps; i++) {
        PE = P[i] - K * EI[i];
        // 如果降水量小于足蒸发需求
        if (PE < 0) {
            R = 0.0;    // 产流总量为零
            RG = 0.0;    // 地下径流量为零
            RS = 0.0;    // 地表径流量为零

            // 如果上层土壤含水量大于蒸发不足量
            if ((WU + PE) > 0.0) {
                // 上层土壤为流域蒸散发提供水量
                EU = K * EI[i];
                // 没有降水量用于增加土壤湿度
                EL = 0.0;          /* 降水用来增加土壤湿度的部分 */
                //
                ED = 0.0;
                // 更新上层土壤含水量
                WU = WU + PE;
            }
                // 上层土壤含水量小于蒸发不足量
            else {
                EU = WU + P[i];        // 上层土壤蒸发,降水全部用于蒸发
                WU = 0.0;                 // 上层含水量为0，全部水分被蒸发
                // 如果下层含水量大于下层土壤的蒸散发潜力
                if (WL > (C * WLM)) {
                    EL = (K * EI[i] - EU) * (WL / WLM);
                    WL = WL - EL;
                    ED = 0;
                }
                    // 如果下层土壤含水量小于下层土壤的蒸散发潜力
                else {
                    // 如果下层土壤的含水量蒸发之后还有剩余
                    if (WL > C * (K * EI[i] - EU)) {
                        EL = C * (K * EI[i] - EU);
                        WL = WL - EL;/////////////////////////////////
                        ED = 0.0;
                    }
                        // 如果下层土壤含水量全部蒸发之后尚不能满足蒸发需求
                    else {
                        EL = WL;              /* 下层土壤含水量全部用于蒸散发 */
                        WL = 0.0;                /* 下层土剩余壤含水量为0        */
                        ED = C * (K * EI[i] - EU) - EL; /* 深层土壤含水量参与蒸发 */
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
            A = WMM * (1 - pow((1.0 - W / WM), 1.0 / (1 + B)));
            // 土壤湿度折算净雨量加上降水后蒸发剩余雨量小于流域内最大含水容量
            if ((A + PE) < this->WMM) {
                // 流域内的产流深度计算
                R = PE             /* 降水蒸发后的剩余量(PE=P-E:状态变量) */
                    + W          /* 流域内土壤湿度 (W:状态变量)         */
                    + WM * pow((1 - (PE + A) / WMM), (1 + B))
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
            if (PE > FC) {
                // 计算地下径流的产流深度
                RG = (R - this->IMP * PE) / PE * FC;
                // 计算地表径流的产流深度
                RS = R - RG;
            }
                // 如果降水经过蒸散发后的剩余量大于等于土壤稳定入渗率
            else {
                // 计算地下径流的产流深度
                RG = R -                /* 总产流深度                         */
                     IMP * PE;        /* 不透水面积上的产流深度，IMP:参数 */
                // 计算地表径流的产流深度
                RS = R - RG;
            }
            /***************      径流划分计算结束      **************/

            // 计算上层土壤蒸散发量
            EU = K *                 /* 流域蒸散发能力与实测蒸散发值的比 */
                 EI[i];           /* 当前时段的水面蒸发               */
            ED = 0.0;
            EL = 0.0;    /* 降水用来增加土壤湿度的部分 */

            /*************** 以下代码负责土壤含水量的更新计算 **************/
            // 如果上层土壤含水量与降水蒸散发剩余量之和减去产流量之后
            // 大于上层土壤的蓄水能力
            if ((WU + PE - R) >= WUM) {
                // 上层含水量+下层含水量+降水剩余量-产流量-上层土壤蓄水需求
                // 后的水量大于下层土壤蓄水需求，多余水量分配到深层土壤
                if ((WU + WL + PE - R - WUM) > WLM) {
                    WU = WUM;                 /* 上层土壤含水量=上层土壤蓄水容量 */
                    WL = WLM;                 /* 下层土壤含水量=下层土壤蓄水容量 */
                    WD = W + PE - R - WU - WL;          /* 绝对降水剩余量补充到深层土壤中  */
                }
                    // 上层含水量+下层含水量+降水剩余量-产流量-上层土壤蓄水需求
                    // 后的水量小于下层土壤蓄水需求，剩余水量补充到下层土壤中
                else {
                    WL = WU + WL + PE - R - WUM;      /* 下层土壤含水量           */
                    WU = WUM;                 /* 上层土壤蓄水容量得到满足 */
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
        /* 以下部分是状态量：总蒸发量、上、下和深层土壤的蒸发的保存 */
/* 1 */    this->E[i] = E;     // 当前步长的蒸发        （模型重要输出）
/* 2 */    this->EU[i] = EU;   // 当前步长上层土壤蒸发
/* 3 */    this->EL[i] = EL;   // 当前步长下层土壤蒸发
/* 4 */    this->ED[i] = ED;   // 当前步长深层土壤蒸发
/* 5 */    this->W[i] = W;     // 当前步长流域平均土壤含水量
/* 6 */    this->WU[i] = WU;   // 当前步长流域上层土壤含水量
/* 7 */    this->WL[i] = WL;   // 当前步长流域下层土壤含水量
/* 8 */    this->WD[i] = WD;   // 当前步长流域深层土壤含水量
/* 9 */    this->RG[i] = RG;   // 当前步长流域基流径流深度
/* 10*/    this->RS[i] = RS;   // 当前步长流域地表径流径流深度
/* 11*/ this->R[i] = R;     // 当前步长的总产流径流深度
    }
    this->Routing();
}

// 保存模拟结果到文件
void XinanjiangModel::SaveResults(char *FileName) {
    int i;
    FILE *fp;
    if ((fp = fopen(FileName, "w")) == NULL) {
        printf("Can not create output file!\n");
        return;
    }
//	fprintf(fp," -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- --------\n");
    fprintf(fp,
            "       E(mm)      EU(mm)      EL(mm)      ED(mm)       W(mm)      WU(mm)      WL(mm)      WD(mm)       R(mm)      RG(mm)      RS(mm)     Q(m3/d)    QS(m3/d)    QG(m3/d)\n");
//	fprintf(fp," -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- -------- --------\n");
    for (i = 0; i < this->m_nSteps; i++) {
        fprintf(fp,
                " %11.5lf %11.5lf %11.5lf %11.5lf %11.5lf %11.5lf %11.5lf %11.5lf %11.5lf %11.5lf %11.5lf %11.5lf %11.5lf %11.5lf\n",
                this->E[i], this->EU[i], this->EL[i], this->ED[i],
                this->W[i], this->WU[i], this->WL[i], this->WD[i],
                this->R[i], this->RS[i], this->RG[i],
                this->Q[i], this->QRS[i], this->QRG[i]);
    }
    fclose(fp);
}

// 进行汇流计算，将径流深度转换为流域出口的流量
void XinanjiangModel::Routing(void) {
    double UH[100]; // 单位线,假定最长的汇流时间为100天
    int N;          // 汇流天数
    double K;       // 汇流参数
    double sum;
    int i, j;

    K = this->KSTOR;
    // 单位线推导
    for (i = 0; i < 100; i++) {
        UH[i] = (1.0 / K) * exp((-1.0 * i) / K);
    }
    sum = 0.0;
    for (i = 0; i < 100; i++) {
        sum += UH[i];
        if (sum > 1.0) {
            UH[i] = 1.0 - (sum - UH[i]);
            N = i;
            break;
        }
    }
    // 单位线汇流计算
    for (i = 0; i < this->m_nSteps; i++) {
        this->QRS[i] = 0.0;
        for (j = 0; j <= N; j++) {
            if ((i - j) < 0) {
                continue;
            }
            this->QRS[i] += this->RS[i - j] * UH[j] * this->m_U;
        }
    }
    //地下水汇流计算
    this->QRG[0] = 0.0;
    for (i = 1; i < this->m_nSteps; i++) {
        this->QRG[i] = this->QRG[i - 1] * this->KKG +
                       this->RG[i] * (1.0 - this->KKG) * this->m_U;
    }
    for (i = 0; i < this->m_nSteps; i++) {
        this->Q[i] = this->QRS[i] + this->QRG[i];
    }
}

void XinanjiangModel::Runoff(double *runoff) {
    /*从1990年1月1日到1996年12月31日为模型的标定期，共有2557天，其总从
      1990年1月1日到1990年12月31日为模型运行的预热期，不参与标定      */
    int i;
    for (i = 0; i < this->m_nSteps; i++) {
        runoff[i] = this->Q[i];
    }
}
