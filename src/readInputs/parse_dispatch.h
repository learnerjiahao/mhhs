//
// Created by wujiahao on 18-5-8.
//

#ifndef MHHSS_PARSE_DISPATCH_H
#define MHHSS_PARSE_DISPATCH_H

#include "rapidjson/filereadstream.h"
#include "rapidjson/prettywriter.h" // for stringify JSON
#include "rapidjson/document.h"
#include "../utils/predefine.h"
#include <cstdio>
#include <string>
#include "../simulation/subbasins_container.h"
#include "../subbasin/simulation_subbasin.h"


class ParseDispatch {

private:
    rapidjson::Document document;
    char readBuffer[65536];
    FILE *fp;
    rapidjson::FileReadStream *is;
    void parseErr(std::string &errMsg);
    void recycle();

public:
    ParseDispatch(std::string dispatchFile);
    ~ParseDispatch();
    void parsingDispatch(SubbasinsContainer &nodes);
};


#endif //MHHSS_PARSE_DISPATCH_H
