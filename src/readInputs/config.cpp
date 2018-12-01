//
// Created by wujiahao on 18-5-7.
//

#include <cpptoml.h>
#include "config.h"
#include "../utils/const.h"
#include "../utils/mpiutil.h"
#include "../utils/datetime_parser.h"

Config* Config::config = nullptr;

const std::string Config::DefaultPickupStrategy = "simple";

const std::string Config::DefaultInputPath = "model_input",
        Config::DefaultOutputPath = "output", Config::DefaultOutputExt = "csv";

const std::string Config::DefaultRunoffModel = "xaj3",
        Config::DefaultRoutingModel = "mskg";

void Config::resolveConfig(std::string configPath) {
    // todo configPath is existed and can be read?
    auto config = cpptoml::parse_file(configPath);

    auto config_dispatch = config->get_table(utils::CONFIG_DISPATCH);
    this->dispatch_file_prefix = config_dispatch->get_as<std::string>(utils::CONFIG_DISPATCH_FILE_PREFIX)
            .value_or(utils::CONFIG_DISPATCH_DEAFULT_FILE_PREFIX);

    // simulation section
    auto confSimulation = config->get_table("simulation");

    auto start_time = confSimulation->get_as<std::string>("start_time");
    if (start_time) {
        this->start_time = start_time.value_or("");
    }
    if (this->start_time.empty()) {
        mpiutil::mpiAbort("config:'start_time' must be set", -1, true);
    }
    this->time_stride = (unsigned long)confSimulation->get_as<int64_t>("time_stride").value_or(24);
    auto timesteps = confSimulation->get_as<int64_t>("time_steps");
    auto end_time = confSimulation->get_as<std::string>("end_time");
    if (timesteps)
    {
        this->sim_timesteps = (unsigned long)timesteps.value_or(0);
        this->end_time = DatetimeParser::getDateTimeStr(this->start_time, this->sim_timesteps, this->time_stride, DatetimeParser::DEFAULT_DATETIME_FORMAT);
    } else if(end_time) {
        this->end_time = end_time.value_or("");
        this->sim_timesteps = DatetimeParser::getTimesteps(this->start_time, this->end_time, DatetimeParser::DEFAULT_DATETIME_FORMAT, this->time_stride);
    }
    if (this->end_time.empty() ) {
        mpiutil::mpiAbort("config:either 'time_steps' or 'end_time' must be set", -1, true);
    }

    this->input_data_path = confSimulation->get_as<std::string>("input_data_path").value_or(Config::DefaultInputPath);

    // models
    auto confModels = config->get_table("models");
    this->global_routting_model = confModels->get_as<std::string>("global_routing_model").
            value_or(Config::DefaultRoutingModel);
    this->global_runoff_model = confModels->get_as<std::string>("global_runoff_model").
            value_or(Config::DefaultRunoffModel);

//    // scheduler section.
//    auto confScheduler = config->get_table("scheduler");
//    this->pickup_strategy = confScheduler->get_as<std::string>("pickup_strategy")
//            .value_or(Config::DefaultPickupStrategy);

//    resolveOutputConfig(config->get_table("output"));
}

Config *Config::getInstance() {
    if (Config::config == nullptr) {
        Config::config = new Config();
    }
    return Config::config;
}

Config::Config() {}

utils::_type_time_step Config::simulationSteps() const {
    return sim_timesteps;
}

std::string Config::getDispatchFilePath(utils::_type_proid proid) const {
    std::stringstream ss;
    ss << dispatch_file_prefix << "_" << proid;
    return ss.str();
}

void Config::resolveOutputConfig(std::shared_ptr<cpptoml::table> output) {
    if (output == nullptr) { // set defalut value if there is no section "output".
        this->output_path = Config::DefaultOutputPath;
        this->output_ext = Config::DefaultOutputExt;
        return;
    }
    this->output_path = output->get_as<std::string>("path").value_or(Config::DefaultOutputPath);
    this->output_ext = output->get_as<std::string>("ext").value_or(Config::DefaultOutputExt);
}
