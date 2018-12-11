//
// Created by wujiahao on 18-7-31.
//

#ifndef PNOHS_ALPHA_SAC_RUNOFF_MODEL_H
#define PNOHS_ALPHA_SAC_RUNOFF_MODEL_H

#include "../model_context.h"
#include "../../readInputs/config.h"
#include "../../simulation/routing_data_meta.h"
#include "../base_model.h"

/**
 * 萨克拉门托產劉模型（SAC模型）
 */
class SACRunoffModel : public BaseModel {
public:

    static const std::string &MODEL_NAME;

private:
    /**
    * 蒸发折算系数 KC
    * 上土层张力水容量 UZTWM/mm
    * 下土层张力水容量 LZTWM/mm
    * 河网、湖泊及水生植物面积占全流域面积的比例 SARVA
    * 永久不透水面积比例 PCTIM
    */
    double &KC, &UZTWM, &LZTWM, &SARVA, &PCTIM;
    /**
     * 上土层自由水日出流系数 UZK
     * 可变不透水面积占全流域面积比例 ADIMP
     * 下土层快速地下水日出流系数 LZSK
     * 下土层慢速地下水日出流系数 LZPK
     */
    double &UZK, &ADIMP, &LZSK, &LZPK;
    /**
     * 下土层慢速地下水容量 LZFPM
     * 下土层快速地下水容量 LZFSM
     * 与最大下渗率有关的参数 ZPERC
     * 下渗曲线参数 REXP
     * 上层自由水容量 UZFWM
     */
    double &LZFPM, &LZFSM, &ZPERC, &REXP, &UZFWM;
    /**
     * 下渗量补充自由水的比例 PFREE
     */
    double &PFREE;
    /**
     * 下土层自由水中不参与蒸散发的比例 RSERV
     */
    double &RSERV;
    /**
     *3个消退系数
     */
    double &CS, &CI, &CG;

    // stat values must be provided
    /**
     * 上土层自由水蓄量 UZFWC
     * 上土层张力水蓄量 UZTWC
     * 下土层张力水蓄量 LZTWC
     * 快速地下水蓄量 LZFSC
     * 慢速地下水蓄量 LZFPC
     */
    double UZFWC, UZTWC, LZTWC, LZFSC, LZFPC;
    /**
     * 流量
     */
    double QRS, QRSS, QRG;

    // 初始状态
    double &UZFWC0, &UZTWC0, &LZTWC0, &LZFSC0, &LZFPC0;
    double &QRS0, &QRSS0, &QRG0;

    std::vector<double> &Ps;
    std::vector<double> &EIs;

public:
    RoutingDataMeta
    runModel(ModelContext &pModelContext, const Config &configValues, const RoutingDataMeta &upRoutDatas,
             int nowTimeStep) override;
    SACRunoffModel(ModelContext *pModelContext);

    std::vector<std::string> getParaNames(const ModelContext *pModelContext) override;

    std::vector<std::string> getInitNames(const ModelContext *pModelContext) override;

    std::vector<std::string> getInputNames(const ModelContext *pModelContext) override;
};


#endif //PNOHS_ALPHA_SAC_RUNOFF_MODEL_H
