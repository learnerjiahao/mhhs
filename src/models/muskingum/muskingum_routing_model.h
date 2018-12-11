//
// Created by wujiahao on 18-7-31.
//

#ifndef PNOHS_ALPHA_MUSKINGEN_ROUTING_MODEL_H
#define PNOHS_ALPHA_MUSKINGEN_ROUTING_MODEL_H

#include <vector>
#include <queue>
#include "../base_model.h"

class MuskingumRoutingModel : public BaseModel {
public:

    MuskingumRoutingModel(ModelContext *pContext);

    std::vector<std::string> getParaNames(const ModelContext *pModelContext) override;

    std::vector<std::string> getInitNames(const ModelContext *pModelContext) override;

    std::vector<std::string> getInputNames(const ModelContext *pModelContext) override;

    static const std::string &MODEL_NAME;
    RoutingDataMeta
    runModel(ModelContext &pModelContext, const Config &configValues, const RoutingDataMeta &upRoutDatas,
             int nowTimeStep) override;

private:
    std::vector<double> preQS34Muskingum;
    std::queue<double> layTimeUpflows;

    double &p_KE;     // 马斯京根演算法的参数（h）	24	取计算时的时间步长即可
    double &p_XE;  // 马斯京根演算法的参数	0.2	0~0.5
    double &p_LT; //  lag time of upstreams flows to downstream
};


#endif //PNOHS_ALPHA_MUSKINGEN_ROUTING_MODEL_H
