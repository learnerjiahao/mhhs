//
// Created by wujiahao on 18-5-6.
//

#include "subbasin.h"
#include "../utils/mpiutil.h"


Subbasin::Subbasin() {}

void Subbasin::addOneUpstream(utils::_type_nodeid upstream_id, utils::_type_proid upstream_locate) {
    this->upstreams[upstream_id] = upstream_locate;
}

void Subbasin::addOneDownstream(utils::_type_nodeid downstream_id, utils::_type_proid downstream_locate) {
    this->downstreams[downstream_id] = downstream_locate;
}

bool Subbasin::isOutlet() {
    return (downstreams.size() == 0);
}

bool Subbasin::isHeadwater() {
    return (upstreams.size() == 0);
}

const utils::_type_proid *Subbasin::getOneUpstreamLocate(utils::_type_nodeid upstreamId) {
    // todo return nullptr
    return &(this->upstreams.at(upstreamId));
}

const utils::_type_proid *Subbasin::getOneDownstreamLocate(utils::_type_nodeid downstreamId) {
    // todo return nullptr
    return &(this->downstreams.at(downstreamId));
}

const utils::_type_proid *Subbasin::getOneDownStreamId(int index) {
    // todo assure downstreams is 0 or 1
    if(isOutlet())
        return nullptr;

    if(index >= downstreams.size()) {
        return nullptr;
    }

    int i = 0;
    for (auto &item : downstreams) {
        if (i == index) {
            return &item.first;
        }
    }
    return nullptr;
}

utils::_type_nodeid Subbasin::upstreamCountNotSameLocate() {
    int count = 0;
    for(auto upstream : upstreams) {
        if(upstream.second != mpiutil::proid)
            count ++;
    }
    return count;
}

utils::_type_nodeid Subbasin::downstreamCountNotSameLocate() {
    int count = 0;
    for(auto downstream : downstreams) {
        if(downstream.second != mpiutil::proid)
            count ++;
    }
    return count;
}

utils::_type_nodeid Subbasin::getNodeid() const {
    return nodeid;
}

void Subbasin::setNodeid(utils::_type_nodeid nodeid) {
    this->nodeid = nodeid;
}

utils::_type_nodeid Subbasin::getDownstreamCount() {
    return this->downstreams.size();
}

utils::_type_nodeid Subbasin::getUpstreamCount() {
    return this->upstreams.size();
}

int Subbasin::getDepth() const {
    return depth;
}

void Subbasin::setDepth(int depth) {
    depth = depth;
}
