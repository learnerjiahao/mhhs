//
// Created by parallels on 11/28/18.
//

#include <gtest/gtest.h>
#include <readInputs/config.h>
#include <models/model_context.h>
#include <models/xaj/xaj_runoff_model.h>

TEST(TEST_MODEL_CONTEXT, TEST_CONFIG) {
    Config *config = Config::getInstance();
    config->resolveConfig("../../inputs/config.toml");
    std::cout << config->start_time << std::endl;
}

TEST(TEST_MODEL_CONTEXT, TEST_MODEL_CONTEXT) {
    Config *config = Config::getInstance();
    config->resolveConfig("../../inputs/config.toml");
    config->input_data_path = "../../inputs/model_input/unit_test";
    ModelContext *pContext = new ModelContext(157);
    pContext->initContext(*config);
    std::cout << config->start_time << std::endl;
}

TEST(TEST_MODEL_CONTEXT, TEST_MODEL_BASE) {
    Config *config = Config::getInstance();
    config->resolveConfig("../../inputs/config.toml");
    config->input_data_path = "../../inputs/model_input/unit_test";
    ModelContext *pContext = new ModelContext(157);
    pContext->initContext(*config);
    std::cout << config->start_time << std::endl;

    BaseModel *pmodel = new XAJRunoffModel(pContext);
    pmodel->getInitNames(pContext);
}

TEST(TEST_MODEL_CONTEXT, TEST_MODEL_XAJ) {
    Config *config = Config::getInstance();
    config->resolveConfig("../../inputs/config.toml");
    config->input_data_path = "../../inputs/model_input/huaihe_subset";
    ModelContext *pContext = new ModelContext(286);
    pContext->initContext(*config);
    BaseModel *pmodel = new XAJRunoffModel(pContext);
    RoutingDataMeta flow;
    RoutingDataMeta upFlow;
    for (int i = 0; i < config->sim_timesteps; ++i) {
        flow = pmodel->runModel(*pContext, *config, upFlow, i + 1);
        std::cout << flow.getFlow() << std::endl;
    }
}