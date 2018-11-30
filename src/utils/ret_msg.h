//
// Created by wujiahao on 2018/11/23.
//

#ifndef MHHSS_RET_MSG_H
#define MHHSS_RET_MSG_H

#include <string>

class RetMSG {
private:
    std::string msg;
    int errCode;
public:
    RetMSG(std::string _msg, int _errCode);
    RetMSG();
    bool isSuccess();
    const std::string &getMsg();
    int getErrCode();
};
#endif //MHHSS_RET_MSG_H
