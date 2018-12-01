//
// Created by parallels on 10/20/18.
//

#ifndef PNOHS_ALPHA_MODEL_WAREHOUSE_H
#define PNOHS_ALPHA_MODEL_WAREHOUSE_H

#include <string>
#include "model_factory.h"
#include "xaj3/xaj3_producer.h"
#include "xaj/xaj_producer.h"
#include "muskingum/mskg_model_producer.h"

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
               ModelFactory::addModelRegistry(XAJ3_MODEL_NAME, XAJ3ModelProducer::getInstance()) &&
               ModelFactory::addModelRegistry(XAJ_MODEL_NAME, XAJModelProducer::getInstance()) &&
//                pModelFactory->addRunoffRegistry(SAC_MODEL_NAME, SACModelProducer::getInstance()) &&
//                // routing models
                ModelFactory::addModelRegistry(MUSKINGUM_MODEL_NAME, MSKGModelProducer::getInstance()); //&&
//                pModelFactory->addRoutingRegistry(DIFFUSIVE_WAVES_MODEL_NAME, DSWVModelProducer::getInstance())
//                ;
        return flag;
    }
}
#endif //PNOHS_ALPHA_MODEL_WAREHOUSE_H
