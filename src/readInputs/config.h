//
// Created by wujiahao on 18-5-7.
//

#ifndef MHHSS_CONFIG_H
#define MHHSS_CONFIG_H


#include <string>
#include "../utils/predefine.h"

class Config {

private:

    static Config *config;
    Config();
    utils::_type_time_step simulation_steps;
    std::string dispatch_file_prefix;

public:
    static Config *getInstance();
    void resolveConfig(std::string configPath);

    utils::_type_time_step simulationSteps() const;
    std::string getDispatchFilePath(utils::_type_proid proid) const;
};


#endif //MHHSS_CONFIG_H
