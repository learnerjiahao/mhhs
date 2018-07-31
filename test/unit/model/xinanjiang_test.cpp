//
// Created by wujiahao on 18-6-21.
//

#include <gtest/gtest.h>
#include <models/base_model.h>
#include <models/xinanjiang3_model.h>
#include <models/xinanjiang2_model.h>

TEST(XAJModel, test1) {
    std::map<std::string, double> initDatas;
    XinAnJiang3Model::createModelInitDatas(initDatas, 0.02, 0.15, 0.01, 0.01, 0.02, 0.01, 0.02);
    std::map<std::string, double> paraDatas;
    XinAnJiang3Model::createModelParaDatas(paraDatas, 200, 2.2, 1.3, 2.4, 0.03, 0.2, 0.23, 0.12, 0.31, 0.11, 0.09, 0.14,
                                          0.04, 0.054, 24, 0.15);
    BaseModel *xajModel = new XinAnJiang3Model(paraDatas, initDatas);
    for (int i = 1; i < 36500; ++i) {
        std::map<std::string, double> inputDatas;
        XinAnJiang3Model::createModelInputDatas(inputDatas, 0.01, 0.002);
        xajModel->runProduceFlowSimul(inputDatas, i);
        RoutingDataMeta routingDataMeta;
        routingDataMeta.setFlow(0.01);
        RoutingDataMeta resultRoutingDataMeta = xajModel->runRouteFlowSimul(inputDatas, i, routingDataMeta);
        resultRoutingDataMeta.getFlow();
    }
    delete xajModel;
}

TEST(XAJModel, test) {
    for (int i = 1; i < 131; ++i) {

        BaseModel *xajModel = new XinAnJiang3Model();

        char pFileName[100];

        std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".ini\0");
        xajModel->loadModelInitDatas(pFileName);

        std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".para\0");
        xajModel->loadModelParaDatas(pFileName);

        std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".inp\0");
        xajModel->loadModelInputDatas(pFileName);
        RoutingDataMeta dataMeta;
        dataMeta.setFlow(0.2);
        xajModel->runProduceFlowSimul(xajModel->getOneSteptimeInputDatas(1), 1);
        xajModel->runRouteFlowSimul(xajModel->getOneSteptimeInputDatas(1), 1, dataMeta);
        delete xajModel;
    }

}

TEST(XAJModel, test3) {
    for (int i = 1; i < 131; ++i) {

        BaseModel *xajModel = new XinAnJiang2Model();

        char pFileName[100];

        std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".ini\0");
        xajModel->loadModelInitDatas(pFileName);

        std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".para\0");
        xajModel->loadModelParaDatas(pFileName);

        std::sprintf(pFileName, "%s%d%s", "../../inputs/model_inputs/", i, ".inp\0");
        xajModel->loadModelInputDatas(pFileName);
        RoutingDataMeta dataMeta;
        xajModel->runProduceFlowSimul(xajModel->getOneSteptimeInputDatas(1), 1);
        RoutingDataMeta dataMeta1 = xajModel->runRouteFlowSimul(xajModel->getOneSteptimeInputDatas(1), 1, dataMeta);
//        printf("%d\n",i);
        delete xajModel;
    }
}