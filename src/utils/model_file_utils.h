//
// Created by wujiahao on 18-9-15.
//

#ifndef PNOHS_ALPHA_MODEL_FILE_UTILS_H
#define PNOHS_ALPHA_MODEL_FILE_UTILS_H


#include <string>
#include <fstream>
#include <sstream>
#include <cstring>
#include <vector>
#include <map>


class ModelFileUtils {
public:

    template <typename T>
    static bool readOneValue(const std::map<std::string, std::string> &valuesMap,
                             T &value, const std::string &valueName, T defaultValue);
    template <typename T>
    static bool readOneValue(const std::map<std::string, std::string> &valuesMap,
                             T &value, const std::string &valueName);

    static std::vector<std::string> splitStr(char *str, const char *delim) {
        std::vector<std::string> strs;
        char *p_str = std::strtok(str, delim);
        while (p_str) {
            strs.push_back(p_str);
            p_str = std::strtok(nullptr, delim);
        }
        return strs;
    };
};

template<typename T>
bool ModelFileUtils::readOneValue(const std::map<std::string, std::string> &valuesMap, T &value,
                                  const std::string &valueName, T defaultValue) {
    bool hadFound = valuesMap.find(valueName) != valuesMap.end();
    if (hadFound)
    {
        std::stringstream ss(valuesMap.at(valueName));
        ss >> value;
    } else {
        value = defaultValue;
    }

    return true;
}

template<typename T>
bool ModelFileUtils::readOneValue(const std::map<std::string, std::string> &valuesMap, T &value,
                                  const std::string &valueName) {
    bool hadFound = valuesMap.find(valueName) != valuesMap.end();
    if (!hadFound)
    {
        return false;
    }
    std::stringstream ss(valuesMap.at(valueName));
    ss >> value;
    return true;
}

//std::vector<std::string> ModelFileUtils::splitStr(char *str, const char *delim) {
//    std::vector<std::string> strs;
//    char *p_str = std::strtok(str, delim);
//    while (p_str) {
//        strs.push_back(p_str);
//        p_str = std::strtok(nullptr, delim);
//    }
//    return strs;
//}


#endif //PNOHS_ALPHA_MODEL_FILE_UTILS_H
