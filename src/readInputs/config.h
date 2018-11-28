//
// Created by wujiahao on 18-5-7.
//

#ifndef MHHSS_CONFIG_H
#define MHHSS_CONFIG_H


#include <string>
#include <cpptoml.h>
#include "../utils/predefine.h"

class Config {

private:

    static Config *config;
    Config();
    void resolveOutputConfig(std::shared_ptr<cpptoml::table> output);

public:
    static Config *getInstance();
    void resolveConfig(std::string configPath);

    utils::_type_time_step simulationSteps() const;
    std::string getDispatchFilePath(utils::_type_proid proid) const;

    utils::_type_time_step sim_timesteps;
    std::string dispatch_file_prefix;


    std::string start_time = "";
    std::string end_time = "";
    unsigned long time_stride = 24;

    std::string input_data_path = "model_input";

    std::string global_runoff_model;
    std::string global_routting_model;

    /** pickup strategy in scheduler.
     * This term specified how the scheduler pickup a runnable simulation node in all available simulation nodes
     * to perform simulation.
     */
    std::string pickup_strategy;

    std::string output_path, output_ext;


    static const std::string DefaultPickupStrategy;
    static const std::string DefaultInputPath, DefaultOutputPath, DefaultOutputExt;

    static const std::string DefaultRunoffModel, DefaultRoutingModel;
};


#endif //MHHSS_CONFIG_H
