//
// Created by wujiahao on 18-9-15.
//

#ifndef PNOHS_ALPHA_DIFFUSIVE_WAVES_ROUTING_MODEL_H
#define PNOHS_ALPHA_DIFFUSIVE_WAVES_ROUTING_MODEL_H


#include "../base_model.h"

class DiffusiveWavesRoutingModel : public BaseModel {

private:
    double &C;   // diﬀusion wave celerity
    double &D;   // diﬀusion coeﬃcient

    const int L_N = 10;
    const long N = 1000;
    double QR0;
    std::vector<double> QR_state;

public:

    static const std::string &MODEL_NAME;

    DiffusiveWavesRoutingModel(ModelContext *pModelContext);

    RoutingDataMeta
    runModel(ModelContext &pModelContext, const Config &configValues, const RoutingDataMeta &upRoutDatas,
             int nowTimeStep) override;

    std::vector<std::string> getParaNames(const ModelContext *pModelContext) override;

    std::vector<std::string> getInitNames(const ModelContext *pModelContext) override;

    std::vector<std::string> getInputNames(const ModelContext *pModelContext) override;

protected:
    double solveSu(double x, double time, double L);
    double solveSd(double x, double time, double L);
    double solveSl(double x, double time, double L);
    double solveK0(double x, double time, double L, double x1);
};


#endif //PNOHS_ALPHA_DIFFUSIVE_WAVES_ROUTING_MODEL_H
