//
// Created by wujiahao on 18-5-6.
//

#ifndef MHHSS_SIMPLE_HBV_PFMODEL_H
#define MHHSS_SIMPLE_HBV_PFMODEL_H

/**
 * Implementation of the model described in:
    * Aghakouchak, Amir, and Emad Habib. "Application of a conceptual hydrologic
    * model in teaching hydrologic processes." International Journal of
    * Engineering Education 26.4 (S1) (2010).
    *
 * Explanation of the model parameters:
    * T_t: Threshold temperature. Decides if snow is melting or accumulating.
    * DD: Degree-day factor. Indicates the decrease of the water content in the snow cover.
    * FC: Field capacity. Describes the maximum soil moisture storage in the subsurface zone.
    * Beta: Shape coefficient. Controls the amount of liquid water (Precipitation + melting Snow), which contributes to runoff.
    * C: Improves model performance, when mean daily temperature deviates considerably from long-term mean.
    * PWP: Permanent Wilting Point. Is a soil-moisture limit for evapotranspiration.
    * K_0: Near surface flow storage coefficient.
    * K_1: Interflow storage coefficient. K_1 should be smaller than K_0.
    * K_2: Baseflow storage coefficient. K_2 should be smaller than K_1.
    * K_p: Percolation storage coefficient.
    * L: Threshold of the water level in the upper storage.
 * Model inputs for simulation:
    * temp: (mean) temperature for one timestep.
    * prec: (summed) precipitation for one timestep. [mm/day]
    * PE_m: long-term mean monthly potential evapotranspiration for one timestep.
    * T_m: long-term mean monthly temperature for one timestep.
 * Basin specification
    * area: Area of the basin in [m²]
 */

class SimpleHBVBFModel {

private:
    double T_t;     // Threshold temperature. Decides if snow is melting or accumulating.
    double DD;      // Degree-day factor. Indicates the decrease of the water content in the snow cover.
    double FC;      // Field capacity. Describes the maximum soil moisture storage in the subsurface zone.
    double Beta;    // Shape coefficient. Controls the amount of liquid water (Precipitation + melting Snow), which contributes to runoff
    double C;       // Improves model performance, when mean daily temperature deviates considerably from long-term mean.
    double PWP;     // Permanent Wilting Point. Is a soil-moisture limit for evapotranspiration.
    double K_0;     // Near surface flow storage coefficient.
    double K_1;     // Interflow storage coefficient. K_1 should be smaller than K_0.
    double K_2;     // Baseflow storage coefficient. K_2 should be smaller than K_1.
    double K_p;     // Percolation storage coefficient.
    double L;       // Threshold of the water level in the upper storage.
public:
    SimpleHBVBFModel(double T_t, double DD, double FC, double Beta, double C, double PWP, double K_0, double K_1,
                   double K_2, double K_p, double L);

public:

    /**

     * 计算指定子流域在指定时间步(时刻t)内的产流
     * @param temp          该子流域在该时间步内(daily)的平均气温(mean temperature)
     * @param prec          该子流域在该时间步内(daily)的降雨或者降雪量(precipitation)
     * @param PE_m          该子流域的该时间步所在月的长期月潜在蒸发量(long-term monthly potential evapotranspiration.)
     * @param T_m           该子流域的该时间步所在月的长期月均气温（long-term monthly temperature.）
     * @param snow_prev     该子流域中t-1时刻的雪覆盖量
     * @param soil_prev     该子流域中t-1时刻中土壤含水量
     * @param s1_prev       该子流域中t-1时刻土壤第一层含水量
     * @param s2_prev       该子流域中t-1时刻土壤第二层含水量
     * @return              该子流域第t时间步内的径流量(mm/d)
     */
    double run_model(const double temp, const double prec, const double PE_m, const double T_m, double &snow_prev,
                     double &soil_prev, double &s1_prev, double s2_prev);
};

#endif //MHHSS_SIMPLE_HBV_PFMODEL_H
