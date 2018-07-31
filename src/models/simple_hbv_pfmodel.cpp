//
// Created by wujiahao on 18-5-6.
//

#include <cmath>
#include "simple_hbv_pfmodel.h"

SimpleHBVBFModel::SimpleHBVBFModel(double T_t, double DD, double FC, double Beta, double C, double PWP, double K_0,
                               double K_1, double K_2, double K_p, double L)
        : T_t(T_t), DD(DD), FC(FC), Beta(Beta),
          C(C), PWP(PWP), K_0(K_0), K_1(K_1),
          K_2(K_2), K_p(K_p), L(L) {}

double SimpleHBVBFModel::run_model(const double temp, const double prec,
                                 const double PE_m, const double T_m, double &snow_prev,
                                 double &soil_prev, double &s1_prev, double s2_prev) {

    double liquid_water;
    double snow, soil, s1, s2;
    double prec_eff, pe, ea;
    double qsim;

    // Check if temperature is below threshold
    if (temp < T_t) {
        //# accumulate snow
        snow = snow_prev + prec;
        //# no liquid water
        liquid_water = 0;
    } else {
        // melt snow
        snow = (0 >= (snow_prev - DD * (temp - T_t)) ? 0 : (snow_prev - DD * (temp - T_t)));
        // add melted snow to available liquid water
        liquid_water = prec + (snow_prev <= DD * (temp - T_t) ? snow_prev : (DD * (temp - T_t)));
    }
    //# calculate the effective precipitation
    prec_eff = liquid_water * pow((soil_prev / FC), Beta);

    // Calculate the potential evapotranspiration
    pe = (1 + C * (temp - T_m)) * PE_m;

    //# Calculate the actual evapotranspiration
    if (soil_prev > PWP)
        ea = pe;
    else
        ea = pe * (soil_prev / PWP);

    // calculate the actual level of the soil reservoir
    soil = soil_prev + liquid_water - prec_eff - ea;

    // calculate the actual level of the near surface flow reservoir
    s1 = s1_prev
         + prec_eff
         - (0 >= (s1_prev - L) ? 0 : (s1_prev - L)) * K_0
         - s1_prev * K_1
         - s1_prev * K_p;

    // calculate the actual level of the base flow reservoir
    s2 = s2_prev
         + s1_prev * K_p
         - s2_prev * K_2;

    qsim = (0 >= (s1_prev - L) ? 0 : (s1_prev - L)) * K_0
           + s1 * K_1
           + s2 * K_2;

    // update *_prev
    snow_prev = snow;
    soil_prev = soil;
    s1_prev = s1;
    s2_prev = s2;

    // rescale qsim from mm/d to m³/d
//    qsim = (qsim * area * 1000) / (24*60*60);

    return qsim;
}