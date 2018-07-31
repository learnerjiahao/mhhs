//
// Created by wujiahao on 18-5-6.
//

#ifndef MHHSS_SUBBASIN_H
#define MHHSS_SUBBASIN_H


#include <vector>
#include "hru.h"
#include <map>

class Subbasin {

protected:
    utils::_type_nodeid nodeid;
    int depth;

    std::map<utils::_type_nodeid, utils::_type_proid> upstreams;
    std::map<utils::_type_nodeid, utils::_type_proid> downstreams;

public:
    utils::_type_nodeid getNodeid() const;
    void setNodeid(utils::_type_nodeid nodeid);
    int getDepth() const;
    void setDepth(int depth);

    Subbasin();
    void addOneUpstream(utils::_type_nodeid, utils::_type_proid);
    void addOneDownstream(utils::_type_nodeid, utils::_type_proid);
    bool isOutlet();   // the size of downstreams is zero
    bool isHeadwater(); // the size of upstareams is zero

    const utils::_type_proid *getOneDownStreamId(int index);
    const utils::_type_proid *getOneUpstreamLocate(utils::_type_nodeid);
    const utils::_type_proid *getOneDownstreamLocate(utils::_type_nodeid);

    utils::_type_nodeid upstreamCountNotSameLocate();
    utils::_type_nodeid getDownstreamCount();
    utils::_type_nodeid getUpstreamCount();

    utils::_type_nodeid downstreamCountNotSameLocate();
};


#endif //MHHSS_SUBBASIN_H
