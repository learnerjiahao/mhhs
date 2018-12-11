//
// Created by wujiahao on 18-7-31.
//

#include <cmath>
#include <fstream>
#include <map>
#include "muskingum_routing_model.h"
#include "../model_warehouse.h"

const std::string &MuskingumRoutingModel::MODEL_NAME = models::MUSKINGUM_MODEL_NAME;
//const std::vector<std::string> MuskingumRoutingModel::paraNames = {"KE", "XE", "LT"};
//const std::vector<std::string> MuskingumRoutingModel::initNames;
//const std::vector<std::string> MuskingumRoutingModel::inputNames;

RoutingDataMeta MuskingumRoutingModel::runModel(ModelContext &pModelContext, const Config &configValues,
                                                const RoutingDataMeta &upRoutDatas, int nowTimeStep) {
    int Kl = 3; // todo Kl == 3 ??? how to choose Kl
    int n = ceil(this->p_KE / (Kl * 1.0)); // 河段数
    double Xl = 0.5 - n * (1 - 2 * this->p_XE) * 0.5;

    double C0 = (0.5 * Kl - Kl * Xl) / (0.5 * Kl + Kl - Kl * Xl);
    double C1 = (0.5 * Kl + Kl * Xl) / (0.5 * Kl + Kl - Kl * Xl);
    double C2 = (-0.5 * Kl + Kl - Kl * Xl) / (0.5 * Kl + Kl - Kl * Xl);

    std::vector<double> nowQS34Muskingum(n + 1);
    nowQS34Muskingum[0] = pModelContext.res_flow;   // 当前时间步，分段马斯京根第一段初始入流I

    double nowUpflows = upRoutDatas.getFlow();

    if (nowTimeStep == 0) {
        while (!layTimeUpflows.empty()) {
            layTimeUpflows.pop();
        }
    }

    layTimeUpflows.push(nowUpflows);
    if (nowTimeStep > this->p_LT) {
        nowQS34Muskingum[0] += layTimeUpflows.front();
        layTimeUpflows.pop();
    }


    for (int i = 1; i < n + 1; ++i) {
        if (nowTimeStep != 1) {
            nowQS34Muskingum[i] =
                    C0 * nowQS34Muskingum[i - 1] + C1 * preQS34Muskingum[i - 1] + C2 * preQS34Muskingum[i];
        } else {
            nowQS34Muskingum[i] = C0 * nowQS34Muskingum[i - 1] + C1 * pModelContext.res_flow + C2 * pModelContext.res_flow;
        }
    }
    pModelContext.res_flow = nowQS34Muskingum[n];  // todo 送到下遊的流量
    preQS34Muskingum.clear();
    preQS34Muskingum = nowQS34Muskingum;
    return RoutingDataMeta(pModelContext.res_flow);
}

MuskingumRoutingModel::MuskingumRoutingModel(ModelContext *pContext) :
                BaseModel(pContext),
                p_KE(pContext->getParamData("KE")),
                p_XE(pContext->getParamData("XE")),
                p_LT(pContext->getParamData("LT")) {
//    paraNames = {"KE", "XE", "LT"};
//    initNames = {};
//    inputNames = {};
}

std::vector<std::string> MuskingumRoutingModel::getParaNames(const ModelContext *pModelContext) {
    return {"KE", "XE", "LT"};
}

std::vector<std::string> MuskingumRoutingModel::getInitNames(const ModelContext *pModelContext) {
    return {};
}

std::vector<std::string> MuskingumRoutingModel::getInputNames(const ModelContext *pModelContext) {
    return {};
}
