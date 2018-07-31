//
// Created by wujiahao on 18-5-7.
//

#ifndef MHHSS_CONTEXT_H
#define MHHSS_CONTEXT_H


#include "readInputs/config.h"
#include "message/routed_data_imsgs_pool.h"

class Context {

public:
    RoutedDataIMsgsPool *IsendMsgsPool;
    RoutedDataIMsgsPool *IrecvMsgsPool;
#ifdef USE_PTHREAD
    pthread_t msgRecvMonitorThread;
#endif

    static Context *getInstance();
    ~Context();

    void parsingConfig(std::string configFilePath);
    void initMsgsPools(utils::_type_nodeid initIsendPoolCap, utils::_type_nodeid initIrecvPoolCap);
    std::string getDispatchFilePath(utils::_type_proid proid);

    utils::_type_time_step getSimulationSteps();

private:
    Config *config;

    Context();
    static Context *context;


    class CGarbo   //它的唯一工作就是在析构函数中删除CSingleton的实例
    {
    public:
        ~CGarbo()
        {
            if(Context::context)
                delete Context::context;
        }
    };
    static CGarbo Garbo;  //定义一个静态成员变量，程序结束时，系统会自动调用它的析构函数
};


#endif //MHHSS_CONTEXT_H
