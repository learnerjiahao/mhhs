//
// Created by parallels on 11/28/18.
//

#include <gtest/gtest.h>
#include <readInputs/config.h>
#include <models/model_context.h>

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