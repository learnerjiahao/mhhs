//
// Created by wujiahao on 2018/11/22.
//

#include <fstream>
#include <cmath>
#include "model_context.h"
#include "../utils/const.h"
#include "../utils/model_file_utils.h"
#include "../utils/datetime_parser.h"

ModelContext::ModelContext(utils::_type_nodeid _nodeid): nodeid(_nodeid) {}

RetMSG ModelContext::initContext(const ConfigValues &configValues) {

    std::string filepath = configValues.input_data_path + '/' + std::to_string(nodeid) + utils::CONFIG_FILE_SUFFIX;
    RetMSG msg = readProperties(filepath, configValues);
    if (!msg.isSuccess())
        return msg;

    filepath = configValues.input_data_path + '/' + std::to_string(nodeid) + utils::PARA_FILE_SUFFIX;
    msg = readMapValues(filepath, params, configValues);
    if (!msg.isSuccess())
        return msg;

    filepath = configValues.input_data_path + '/' + std::to_string(nodeid) + utils::INIT_FILE_SUFFIX;
    msg = readMapValues(filepath, initDatas, configValues);
    if (!msg.isSuccess())
        return msg;

    filepath = configValues.input_data_path + '/' + std::to_string(nodeid) + utils::INPUT_FILE_SUFFIX;
    msg = readInputDatas(filepath, configValues);
    if (!msg.isSuccess())
        return msg;

    if (hadObser) {
        filepath = configValues.input_data_path + '/' + std::to_string(nodeid) + utils::INPUT_FILE_SUFFIX;
        msg = readObserDatas(filepath, configValues);
        if (!msg.isSuccess())
            return msg;
    }

    return RetMSG();
}

RetMSG ModelContext::readMapValues(const std::string &filePath, std::map<std::string, double> &dataMap, const ConfigValues &configValues) {
    std::ifstream ifs(filePath);
    if (!ifs.good()) {
        return RetMSG(filePath + " not exist!", -1);
    }
    std::string name;
    double value;
    while (ifs >> name >> value) {
        dataMap[name] = value;
    }
    ifs.close();
    return RetMSG();
}

RetMSG ModelContext::readInputDatas(const std::string &filePath, const ConfigValues &configValues) {
    std::ifstream ifs_input(filePath);
    if (!ifs_input.good()) {
        return RetMSG(filePath + " not exist!", -1);
    }
    std::string TM, stime;
    char buff[2048] = {0};
    if (!ifs_input.getline(buff, 2048)) {
        return RetMSG("the first row is not a head in file " + filePath, -1);
    }
    std::vector<std::string> heads = ModelFileUtils::splitStr(buff, " \t");

    unsigned long now_steps = 0;
    while (now_steps < configValues.sim_timesteps
           && ifs_input.getline(buff, 2048)) {
        std::string real_TM = DatetimeParser::getDateTimeStr(configValues.start_time, now_steps,
                                                             configValues.time_stride,
                                                             DatetimeParser::DEFAULT_DATETIME_FORMAT);
        std::vector<std::string> dstrs = ModelFileUtils::splitStr(buff, " \t");
        for (int i = 2; i < dstrs.size(); ++i) {
            std::stringstream ss(dstrs[i]);
            double data;
            ss >> data;
            inputDatas[heads[i-1]].push_back(data);
        }
        TM = dstrs[0] + " " + dstrs[1];
        if (TM != real_TM)
            continue;

        now_steps ++;
    }
    ifs_input.close();
    if (now_steps != configValues.sim_timesteps) {
        return RetMSG("in file " + filePath + ": should had " +
               std::to_string(configValues.sim_timesteps)
               + " rows's datas from " +
               configValues.start_time +
               " to " + configValues.end_time, -1);
    }
    return RetMSG();
}

RetMSG ModelContext::readProperties(const std::string &filePath, const ConfigValues &configValues) {
    std::ifstream ifs(filePath);
    if (!ifs.good()) {
        return RetMSG(filePath + " not exist!", -1);
    }
    std::map<std::string, std::string> valueMap;
    std::string name;
    std::string value;
    while (ifs >> name >> value) {
        valueMap[name] = value;
    }
    ifs.close();

    if (!ModelFileUtils::readOneValue(valueMap, area, "area")) {
        return RetMSG("can't find 'area' property in file " + filePath , -1);
    }

    ModelFileUtils::readOneValue(valueMap, hadObser, "has_obser_data", false);

    ModelFileUtils::readOneValue(valueMap, length, "len", sqrt(area) / 3.1415926);

    ModelFileUtils::readOneValue(valueMap, latitude, "latitude", 0.0);
    ModelFileUtils::readOneValue(valueMap, longitude, "longitude", 0.0);
    ModelFileUtils::readOneValue(valueMap, elev, "elev", 0.0);

    ModelFileUtils::readOneValue(valueMap, runoffModel, "runoff_model", configValues.global_runoff_model);
    ModelFileUtils::readOneValue(valueMap, routingModel, "routting_model", configValues.global_routting_model);

    return RetMSG();
}

RetMSG ModelContext::readObserDatas(const std::string &filePath, const ConfigValues &configValues) {
    std::ifstream ifs_input(filePath);
    std::string TM, stime;
    double obser_data;
    unsigned long now_steps = 0;
    ifs_input >> TM >> TM;  // ignore head row
    while (now_steps < configValues.sim_timesteps && ifs_input >> TM >> stime >> obser_data) {
        std::string real_TM = DatetimeParser::getDateTimeStr(configValues.start_time, now_steps,
                                                             configValues.time_stride,
                                                             DatetimeParser::DEFAULT_DATETIME_FORMAT);
        TM += (" " + stime);
        if (TM != real_TM)
            continue;
        obserDatas.push_back(obser_data);
        now_steps ++;
    }
    ifs_input.close();

    if (now_steps != configValues.sim_timesteps) {
        return RetMSG("in file " + filePath + ": should had " + std::to_string(configValues.sim_timesteps)
                    + " rows's datas from " + configValues.start_time + " to " + configValues.end_time, -1);
    } else {
        return RetMSG();
    }
}
