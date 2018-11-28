//
// Created by wujiahao on 18-5-6.
//

#include "hru.h"

HRU::HRU(utils::_type_hruid hruid) : hruid(hruid) {}

void HRU::HRUSimulation(RoutingDataMeta &HRUPLRoutingData) {
    // 构造模型
    double T_t = 0, DD = 4.25,
            FC = 177.1, Beta = 2.35,
            C = 0.02, PWP = 105.89,
            K_0 = 0.05, K_1 = 0.03,
            K_2 = 0.02, K_p = 0.05,
            L = 4.87;
    SimpleHBVBFModel hbvModel(T_t, DD, FC, Beta, C, PWP, K_0, K_1, K_2, K_p, L);
    double snow_prev = 10, soil_prev = 100, s1_prev = 3.2, s2_prev = 10;
    double flow = hbvModel.run_model(20.7, 0, 4.3, 2.1, snow_prev, soil_prev, s1_prev, s2_prev);

    HRUPLRoutingData.setFlow(1.0);
}
