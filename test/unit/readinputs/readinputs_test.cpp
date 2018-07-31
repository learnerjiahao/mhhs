//
// Created by wujiahao on 18-5-7.
//

#include <gtest/gtest.h>
#include <readInputs/config.h>
#include <readInputs/parse_dispatch.h>


TEST(config_test, config_test_read) {
    Config *config = Config::getInstance();
    config->resolveConfig("../../inputs/config.toml");
    std::cout << std::endl << config->simulationSteps() << " " << config->getDispatchFilePath(1) << std::endl;
    Config *config1 = Config::getInstance();
    std::cout << std::endl << config1->simulationSteps() << " " << config1->getDispatchFilePath(1) << std::endl;

}


TEST(parse_dispatch_test, parse_dispatch_test) {
//#define NO_MPI "ON"
    SubbasinsContainer *nodes = SubbasinsContainer::getInstance(10);
    ParseDispatch pd("../../inputs/dispatch/dispatch.json_0");
    pd.parsingDispatch(*nodes);
    TRoutingData sendTRoutingData;
    for (auto node : *(nodes->subbasins)) {

        if(node.second.isOutlet())
            continue;

//        sendTRoutingData.updateData(node.second.getRoutingData2Downstream(),
//                                    *node.second.getOneDownStreamId(),
//                                    node.second.getNowSimuStep());
        std::cout << *(node.second.getOneDownstreamLocate(sendTRoutingData.getDownstreamId()))
                  << ":::::" << sendTRoutingData.getTimeStep()
                  << "----" << sendTRoutingData.getDownstreamId() << "----" << sendTRoutingData.getFlow() << std::endl;
    }
}