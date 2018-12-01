//
// Created by wujiahao on 18-5-7.
//

#include <iostream>
#include <args.h>
#include <thread>
#include "simulation.h"
#include "../utils/mpiutil.h"
#include "../readInputs/parse_dispatch.h"
#include "../models/base_model.h"
#include "../models/model_producer.h"
#include "../models/model_factory.h"
#include "../models/model_warehouse.h"

Context *Simulation::context = Context::getInstance();
SubbasinsContainer *Simulation::subbasinsCt = nullptr;

void Simulation::initSimulation(int argc, char *argv[]) {
    // init mpi
    mpiutil::mpiInit(argc, argv);
    // init context
    args::ArgumentParser parser("This is a high performance computing hydrological simulationprogram.", "");
    args::HelpFlag help(parser, "help", "Display this help menu", {'h', "help"});
    args::ValueFlag<std::string> configPath(parser, "Filepath", "The prefix of configure filepath", {'c', "config"});
    try {
        parser.ParseCLI(argc, argv);
    }
    catch (args::Help) {
        std::stringstream ssmsg;
        ssmsg << "Usage Err!\n" << parser;
        mpiutil::mpiAbort(ssmsg.str(), true, -1);
    }
    catch (args::ParseError e) {
        std::stringstream ssmsg;
        ssmsg << "Usage Err!\n" << e.what() << "\n" << parser;
        mpiutil::mpiAbort(ssmsg.str(), true, -1);
    }
    catch (args::ValidationError e) {
        std::stringstream ssmsg;
        ssmsg << "Usage Err!\n" << e.what() << "\n" << parser;
        mpiutil::mpiAbort(ssmsg.str(), true, -1);
    }
    if (!configPath) {
        std::stringstream ssmsg;
        ssmsg << "Usage Err!\n" << parser;
        mpiutil::mpiAbort(ssmsg.str(), true, -1);
    }
    context->parsingConfig(args::get(configPath));

    // init subbasinsCt
    subbasinsCt = SubbasinsContainer::getInstance(context->getSimulationSteps());

}

void Simulation::prepareSimulation() {

    // 1.load dispatch result datas
    ParseDispatch pd(context->getDispatchFilePath(mpiutil::proid));
    pd.parsingDispatch(*subbasinsCt);

    // 2. init subbasinsCt
    subbasinsCt->initSortCt();

    // 3.init model warehouse
    models::registryModelProducers();

    // 3.init model
    for (auto &node : *(subbasinsCt->subbasins)) {
        ModelContext *pContext = new ModelContext(node.first);
        pContext->initContext(*Config::getInstance());
        BaseModel *runoffModel = ModelFactory::prodeceRunoffModel(pContext);
        RetMSG msg = runoffModel->checkModelDatas(pContext);
        if (!msg.isSuccess()) {
            mpiutil::mpiAbort(msg.getMsg(), msg.getErrCode(), false);
        }
        BaseModel *routingModel = ModelFactory::prodeceRoutingModel(pContext);
        msg = routingModel->checkModelDatas(pContext);
        if (!msg.isSuccess()) {
            mpiutil::mpiAbort(msg.getMsg(), msg.getErrCode(), false);
        }
        node.second.setModelContext(pContext);
        node.second.setModels(runoffModel, routingModel);
    }

    // 4.init msgsPool's initial capacity
    context->initMsgsPools(subbasinsCt->subbasinsSize(), subbasinsCt->solveUpstreamsNeedComm());

#ifdef USE_PTHREAD
    // init pthread
    pthread_create(&context->msgRecvMonitorThread, nullptr, threadRecvMsgFun, subbasinsCt);
    pthread_detach(context->msgRecvMonitorThread);
#endif

}

void Simulation::runSimulation() {
    while (!subbasinsCt->isAllFinished()) {
#ifndef USE_PTHREAD
        std::vector<TRoutingData> recvTRoutingDatas;
//        context->IrecvMsgsPool->chooseAllAvail2IrecvAndSaveFinNoWait(recvTRoutingDatas);
        context->IrecvMsgsPool->chooseAllAvail2IrecvAndSaveFinNoWaitButDy(recvTRoutingDatas);
        subbasinsCt->appendRoutingDatas(recvTRoutingDatas);
#endif
        SimulationSubbasin *allSimulatableSubbasin = subbasinsCt->pickAllSimulatableComplex();
//        SimulationSubbasin *allSimulatableSubbasin = subbasinsCt->pickAllSimulatable();
        if (allSimulatableSubbasin != nullptr) {
            // do simulation
            const std::map<utils::_type_nodeid, RoutingDataMeta> &sendRoutingDatas = allSimulatableSubbasin->runOneSimulation();
            if (!allSimulatableSubbasin->isOutlet()) {
                // todo if allSimulatableSubbasin has more than one downstream?
                for(auto sendRoutingData : sendRoutingDatas) {
                    TRoutingData sendTRoutingData(sendRoutingData.first,allSimulatableSubbasin->getNowSimuStep() - 1);
                    sendTRoutingData.updateMetaData(sendRoutingData.second);
                    // MPI_ISend or memory copy directly
                    utils::_type_proid targetProid =
                            *allSimulatableSubbasin->getOneDownstreamLocate(sendRoutingData.first);
                    if (targetProid != mpiutil::proid) {
#ifndef USE_PTHREAD
                        context->IsendMsgsPool->chooseAvailableAndIsendNoWait(targetProid,sendTRoutingData);
#else
    #ifdef USE_NOBLOCKING_SEND
                        context->IsendMsgsPool->chooseAvailableAndIsendNoWait(targetProid,sendTRoutingData);
    #else

        #ifdef USE_HNOBLOCKING_SEND
                        context->IsendMsgsPool->chooseAvailableAndIsendMayWait(targetProid,sendTRoutingData);
        #else
                        MPI_Send(&sendTRoutingData, sizeof(TRoutingData),
                                 MPI_BYTE, targetProid, mpiutil::MPI_ROUTING_MSG_TAG, MPI_COMM_WORLD);
        #endif

    #endif

#endif
                    } else {
                        subbasinsCt->appendRoutingData(sendTRoutingData);
                    }
                }
            }
            subbasinsCt->updateSortCt(allSimulatableSubbasin->getNowSimuStep()-1, allSimulatableSubbasin->getNodeid());
            continue;
        }
        SimulationSubbasin *slopeSimulatableSubbasin = subbasinsCt->pickSlopeSimulatable();
        if (slopeSimulatableSubbasin != nullptr) {
            slopeSimulatableSubbasin->slopePLSimulation();
        }
    }
}

void Simulation::finishSimulation() {
//    for (auto &node : *subbasinsCt) {
//        delete(model.second);
//    }
    delete (subbasinsCt);
    delete (context);
    mpiutil::mpiFinish();
}

void Simulation::run(int argc, char **argv) {
    initSimulation(argc, argv);
    prepareSimulation();
    double startTime = MPI_Wtime();
    runSimulation();
    double runTime =  MPI_Wtime() - startTime;
    double trueRunTime;
    MPI_Reduce(&runTime, &trueRunTime, 1, MPI_DOUBLE, MPI_MAX, mpiutil::masterProid, MPI_COMM_WORLD);
    if(mpiutil::proid == mpiutil::masterProid) {
        printf("simulation had spent %fs\n", trueRunTime);
    }
    finishSimulation();
}

#ifdef USE_PTHREAD
void *Simulation::threadRecvMsgFun(void *sct) {

    SubbasinsContainer *trueACT = subbasinsCt; //static_cast<SubbasinsContainer *>(sct);
    unsigned long hadCommCount = 0;
    unsigned long allCommCount = trueACT->getAllCommucationCount();

#ifdef USE_HNOBLOCKING_RECV
    std::vector<TRoutingData> recvTRoutingDatas;
#else
    TRoutingData recvTRoutingData;
#endif

    while (hadCommCount < allCommCount) {
        // blocking recv to save cpu resource
#ifdef USE_HNOBLOCKING_RECV
        context->IrecvMsgsPool->chooseAllAvail2IrecvAndSaveFinMayWait(recvTRoutingDatas);
        subbasinsCt->appendRoutingDatas(recvTRoutingDatas);
        hadCommCount += recvTRoutingDatas.size();
        recvTRoutingDatas.clear();
#else
        MPI_Recv(&recvTRoutingData, sizeof(TRoutingData),
                 MPI_BYTE, MPI_ANY_SOURCE, mpiutil::MPI_ROUTING_MSG_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        trueACT->appendRoutingData(recvTRoutingData);
        hadCommCount++;
#endif

#ifdef DEV_MODE1
        std::stringstream ssMsg;
        ssMsg << "P" << mpiutil::proid << ": recv data whose target nodeid is "
              << recvTRoutingData.getDownstreamId()
              << " timestep=" << recvTRoutingData.getTimeStep() << " flow=" << recvTRoutingData.getFlow() << std::endl;
        printf(ssMsg.str().c_str());
#endif
    }
    return nullptr;
}
#endif


