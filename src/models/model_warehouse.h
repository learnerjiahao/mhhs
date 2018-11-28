//
// Created by parallels on 10/20/18.
//

#ifndef PNOHS_ALPHA_MODEL_WAREHOUSE_H
#define PNOHS_ALPHA_MODEL_WAREHOUSE_H

#include <string>
#include "model_factory.h"
#include "xaj3/xaj3_producer.h"

namespace models {

    // runoff models's names
    const static std::string XAJ3_MODEL_NAME = "xaj3";
    const static std::string XAJ_MODEL_NAME = "xaj";
    const static std::string SAC_MODEL_NAME = "sac";
    // routing models's names
    const static std::string MUSKINGUM_MODEL_NAME = "mskg";
    const static std::string DIFFUSIVE_WAVES_MODEL_NAME = "dswv";

    // init model prodecer
    static bool registryModelProducers() {
       bool flag =
                // runoff models
                ModelFactory::addModelRegistry(XAJ3_MODEL_NAME, XAJ3ModelProducer::getInstance()) //&&
//                pModelFactory->addRunoffRegistry(SAC_MODEL_NAME, SACModelProducer::getInstance()) &&
//                // routing models
//                pModelFactory->addRoutingRegistry(MUSKINGUM_MODEL_NAME, MSKGModelProducer::getInstance()) &&
//                pModelFactory->addRoutingRegistry(DIFFUSIVE_WAVES_MODEL_NAME, DSWVModelProducer::getInstance())
                ;
        return flag;
    }
}
#endif //PNOHS_ALPHA_MODEL_WAREHOUSE_H
