//
// Created by wujiahao on 18-5-7.
//

#include "context.h"

Context *Context::context = nullptr;

Context::Context() {
    config = Config::getInstance();
    IsendMsgsPool = nullptr;
}

Context *Context::getInstance() {

    if(Context::context == nullptr)
        Context::context = new Context();

    return Context::context;
}

Context::~Context() {
    delete config;
    delete IsendMsgsPool;
}

void Context::parsingConfig(std::string configFilePath) {
    config->resolveConfig(configFilePath);
}

void Context::initMsgsPools(utils::_type_nodeid initIsendPoolCap, utils::_type_nodeid initIrecvPoolCap) {
    IsendMsgsPool = new RoutedDataIMsgsPool(initIsendPoolCap);
    IrecvMsgsPool = new RoutedDataIMsgsPool(initIrecvPoolCap);
}

std::string Context::getDispatchFilePath(utils::_type_proid proid) {
    return config->getDispatchFilePath(proid);
}

utils::_type_time_step Context::getSimulationSteps() {
    return config->simulationSteps();
}

