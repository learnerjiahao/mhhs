//
// Created by wujiahao on 18-5-7.
//

#include <cpptoml.h>
#include "config.h"
#include "../utils/const.h"
#include "../utils/mpiutil.h"

Config* Config::config = nullptr;

void Config::resolveConfig(std::string configPath) {
    // todo configPath is existed and can be read?
    auto config = cpptoml::parse_file(configPath);

    auto config_simulation = config->get_table(utils::CONFIG_SIMULATION);
    this->simulation_steps = config_simulation->get_as<utils::_type_time_step>(utils::CONFIG_SIMULATION_STEPS).value_or(1);

    auto config_dispatch = config->get_table(utils::CONFIG_DISPATCH);
    this->dispatch_file_prefix = config_dispatch->get_as<std::string>(utils::CONFIG_DISPATCH_FILE_PREFIX)
            .value_or(utils::CONFIG_DISPATCH_DEAFULT_FILE_PREFIX);
}

Config *Config::getInstance() {
    if(config == nullptr)
        config = new Config();
    return config;
}

Config::Config() {}

utils::_type_time_step Config::simulationSteps() const {
    return simulation_steps;
}

std::string Config::getDispatchFilePath(utils::_type_proid proid) const {
    std::stringstream ss;
    ss << dispatch_file_prefix << "_" << proid;
    return ss.str();
}
