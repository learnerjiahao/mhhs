//
// Created by parallels on 11/28/18.
//

#ifndef MHHSS_XAJ3_RUNOFF_MODEL_H
#define MHHSS_XAJ3_RUNOFF_MODEL_H


#include "../base_model.h"

class XAJ3RunoffModel : public BaseModel {
private:
    // params data

    // state(init) data
public:
    RoutingDataMeta
    runModel(const ModelContext &pModelContext, const ConfigValues &configValues, const RoutingDataMeta &upRoutDatas,
             int nowTimeStep) override;

    XAJ3RunoffModel(const ModelContext *pModelContext);

    std::vector<std::string> getParaNames() override;

    std::vector<std::string> getInitNames() override;

    std::vector<std::string> getInputNames() override;
};


#endif //MHHSS_XAJ3_RUNOFF_MODEL_H
