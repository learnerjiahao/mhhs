//
// Created by wujiahao on 18-5-8.
//


#include "parse_dispatch.h"
#include "../utils/mpiutil.h"
#include "../simulation/subbasins_container.h"
//#define NO_MPI


ParseDispatch::ParseDispatch(std::string dispatchFile) {

    fp = fopen(dispatchFile.c_str(), "r");
    if(fp == nullptr) {
        std::string errMsg = "dispatch file[" + dispatchFile + "] not existed or can't be readable!!!";
        parseErr(errMsg);
    }

    is = new rapidjson::FileReadStream(fp, readBuffer, sizeof(readBuffer));

    if(document.ParseStream(*is).HasParseError()) {
        recycle();
        std::string errMsg = "dispatch file[" + dispatchFile + "] is not a corrected json format file!!!";
        parseErr(errMsg);
    }


}

ParseDispatch::~ParseDispatch() {
    recycle();
}

void ParseDispatch::parsingDispatch(SubbasinsContainer &nodes) {

    if(!document.HasMember("prank") || !document["prank"].IsInt()
       || !document.HasMember("proid") || !document["proid"].IsInt()
       || !document.HasMember("nrank") || !document["nrank"].IsInt()
       || !document.HasMember("nodes") || !document["nodes"].IsArray())  {
        recycle();
        std::string errMsg = "the dispatch file is not a corrected json format file!!!";
        parseErr(errMsg);
    }
    utils::_type_proid prorank = document["prank"].GetInt();
    utils::_type_proid proid = document["proid"].GetInt();

#ifndef NO_MPI
    if(prorank != mpiutil::proRanks || proid != mpiutil::proid) {
        recycle();
        std::string errMsg = "the 'prank' or 'proid' in this dispatch file is not equal to this running env !!!";
        parseErr(errMsg);
    }
#endif
    utils::_type_proid nrank = document["nrank"].GetInt();
    const rapidjson::Value &nodesV = document["nodes"];
#ifndef NO_MPI
    if(nrank != nodesV.Size()) {
        recycle();
        std::string errMsg = "the 'nrank' in this dispatch file is not equal to the count of 'nodes' !!!";
        parseErr(errMsg);
    }
#endif

    for(rapidjson::SizeType i = 0; i < nodesV.Size(); i++) {

        // todo check item' name and type is vaild or not
        utils::_type_nodeid nodeid = nodesV[i]["node_id"].GetInt();
        int depth = nodesV[i]["depth"].GetInt();

        SimulationSubbasin subbasin;

        const rapidjson::Value &upstreamsV = nodesV[i]["upstreams"];
        for(rapidjson::SizeType j = 0; j < upstreamsV.Size(); j++) {
            subbasin.addOneUpstream(upstreamsV[j]["node_id"].GetInt(), upstreamsV[j]["locate"].GetInt());
        }

        const rapidjson::Value &downstreamsV = nodesV[i]["downstreams"];
        for(rapidjson::SizeType j = 0; j < downstreamsV.Size(); j++) {
            subbasin.addOneDownstream(downstreamsV[j]["node_id"].GetInt(), downstreamsV[j]["locate"].GetInt());
        }
        subbasin.setNodeid(nodeid);
        subbasin.setDepth(depth);

        nodes.addNewSubbasin(nodeid, subbasin);
    }

}

void ParseDispatch::parseErr(std::string &errMsg) {
#ifdef NO_MPI
    std::cerr << errMsg << std::endl;
    exit(-1);
#else
    mpiutil::mpiAbort(errMsg, -1, false);
#endif
}

void ParseDispatch::recycle() {
    fclose(fp);
    delete(is);
}
