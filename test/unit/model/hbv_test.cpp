//
// Created by wujiahao on 18-5-6.
//

//
// Created by wujiahao on 2018/4/6.
//

#include <iostream>
#include <fstream>
#include <gtest/gtest.h>
#include <models/simple_hbv_pfmodel.h>

/**
 * 测试 hbv modeli(一个子流域上)
 */
TEST(model_test, model_test_simple_hbv) {

    // 读入模型所需数据
    std::ifstream daily_datas("../../inputs/hbv_daily_inputs.txt");
    std::ifstream monthly_datas("../../inputs/hbv_monthly_inputs.txt");
    std::vector<double> temps, prevs;
    std::vector<std::string> dates;
    std::vector<int> months;
    double temp, prev;
    int month;
    std::string date;
    while (!daily_datas.eof()) {
        daily_datas >> date >> month >> temp >> prev;
        temps.push_back(temp);
        prevs.push_back(prev);
        months.push_back(month - 1);
        dates.push_back(date);
    }
    daily_datas.close();
    std::vector<double> monthly_temps, monthly_evaps;
    double monthly_temp, monthly_evap;
    int not_needed;
    while (!monthly_datas.eof()) {
        monthly_datas >> monthly_temp >> not_needed >> monthly_evap;
        monthly_temps.push_back(monthly_temp);
        monthly_evaps.push_back(monthly_evap);
    }
    monthly_datas.close();

    // 构造模型
    double T_t = 0, DD = 4.25,
            FC = 177.1, Beta = 2.35,
            C = 0.02, PWP = 105.89,
            K_0 = 0.05, K_1 = 0.03,
            K_2 = 0.02, K_p = 0.05,
            L = 4.87;
    SimpleHBVBFModel hbvModel(T_t, DD, FC, Beta, C, PWP, K_0, K_1, K_2, K_p, L);

    // 单个子流域进行各个时刻的径流模拟
    std::ofstream runoff_datas("hbv_runoff.txt", std::ios::trunc);
    runoff_datas << "date" << "\t" << "runoff(mm/d)" << std::endl;
    double snow_prev = 0, soil_prev = 100, s1_prev = 3, s2_prev = 10;
    double runoff;
    for (unsigned long i = 0; i < temps.size(); i++) {
        runoff = hbvModel.run_model(temps[i], prevs[i],
                                    monthly_evaps[months[i]], monthly_temps[months[i]],
                                    snow_prev, soil_prev, s1_prev, s2_prev);
        runoff_datas << dates[i] << "\t" << runoff << std::endl;
    }
    runoff_datas.close();

}

