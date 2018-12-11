//
// Created by wujiahao on 18-9-15.
//

#include "diffusive_waves_routing_model.h"
#include "../../utils/model_file_utils.h"
#include "../model_warehouse.h"
#include <cmath>

const std::string &DiffusiveWavesRoutingModel::MODEL_NAME = models::DIFFUSIVE_WAVES_MODEL_NAME;

double DiffusiveWavesRoutingModel::solveSu(double x, double time, double L) {
    x = x * 1000;
    time = time * 3600;
    L = L * 1000;
    double PI = 3.1415926;
    double res = exp(C * x / (2 * D));
    double sum = 0.0, pre_sum = 0.0;
    unsigned long iter_n = 1;
    do {
        pre_sum = sum;
        sum = sum + (8 * iter_n * PI * D * D) /
                    (C * C * L * L + 4 * iter_n * iter_n * PI * PI * D * D) *
                    (1 - exp(-(C * C * L * L + 4 * iter_n * iter_n * PI * PI * D * D) /
                             (4 * D * L * L) * time)) * sin(iter_n * PI * x / L);
//        printf("%lf, %lf, %lf\n",pre_sum, sum, fabs(pre_sum - sum));
    } while (++iter_n <= N);

    res *= sum;
    return res;
}

double DiffusiveWavesRoutingModel::solveSd(double x, double time, double L) {
    x = x * 1000;
    time = time * 3600;
    L = L * 1000;
    double PI = 3.1415926;
    double res = exp(C * (x - L) / (2 * D));
    double sum = 0.0;

    unsigned long iter_n = 1;
    do {
        sum = sum + (pow(-1, iter_n + 1) * 8 * iter_n * PI * D * D) /
                    (C * C * L * L + 4 * iter_n * iter_n * PI * PI * D * D) *
                    (1 - exp(-(C * C * L * L + 4 * iter_n * iter_n * PI * PI * D * D) /
                             (4 * D * L * L) * time)) * sin(iter_n * PI * x / L);
    } while (++iter_n <= N);

    res *= sum;
    return res;
}

double DiffusiveWavesRoutingModel::solveSl(double x, double time, double L) {
    return solveSu(x, time, L);
}

double DiffusiveWavesRoutingModel::solveK0(double x, double time, double L, double x1) {
    x = x * 1000;
    time = time * 3600;
    L = L * 1000;
    double PI = 3.1415926;
    double res = exp(C * (x - x1) / (2 * D) - (C * C * time / (4 * D))) * 2 / L;
    double sum = 0.0;
    unsigned long iter_n = 1;
    do {
        sum = sum + exp(-(iter_n * iter_n * PI * PI * D) / (L * L) * time) * sin(iter_n * PI * x1 / L) *
                    sin(iter_n * PI * x1 / L);
    } while (++iter_n <= N);

    res *= sum;
    return res;
}

DiffusiveWavesRoutingModel::DiffusiveWavesRoutingModel(ModelContext *pModelContext) :
        BaseModel(pModelContext),
        C(pModelContext->getParamData("C")),
        D(pModelContext->getParamData("D")) { }

RoutingDataMeta DiffusiveWavesRoutingModel::runModel(ModelContext &pModelContext, const Config &configValues,
                                                     const RoutingDataMeta &upRoutDatas, int nowTimeStep) {
    double inflow = pModelContext.res_flow;
    double upFlows = 0.0;
    upFlows += upRoutDatas.getFlow();

    double interval_len = pModelContext.area / L_N;

    std::vector<double> nowQRs(L_N);
    nowQRs[0] = upFlows;

    for (int i = 0; i < L_N; ++i) {

        double Su = solveSu(i * interval_len, configValues.time_stride * 1.0, pModelContext.area);
        double Sl = solveSl(i * interval_len, configValues.time_stride * 1.0, pModelContext.area);
        double flow = upFlows * Su + inflow * Sl;

        double Sd = solveSd(i * interval_len, configValues.time_stride * 1.0, pModelContext.area);
        if (nowTimeStep == 0) {
            flow += QR0 * Sd;
        } else {
            flow += QR_state[L_N - 1] * Sd;
        }

        for (int j = 0; j < L_N; ++j) {
            double K0 = solveK0(i * interval_len, configValues.time_stride * 1.0, pModelContext.area, j * interval_len);
            if (nowTimeStep == 0) {
                flow += QR0 * K0 * interval_len;
            } else {
                flow += QR_state[j] * K0 * interval_len;
            }
        }
        nowQRs[i] = flow;
    }

    QR_state = nowQRs;
    pModelContext.res_flow = QR_state[L_N-1];
    return RoutingDataMeta(pModelContext.res_flow);
}

std::vector<std::string> DiffusiveWavesRoutingModel::getParaNames(const ModelContext *pModelContext) {
    return {"C", "D"};
}

std::vector<std::string> DiffusiveWavesRoutingModel::getInitNames(const ModelContext *pModelContext) {
    return {};
}

std::vector<std::string> DiffusiveWavesRoutingModel::getInputNames(const ModelContext *pModelContext) {
    return {};
}
