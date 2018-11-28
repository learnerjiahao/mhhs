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

RetMSG::RetMSG(std::string _msg, int _errCode): msg(_msg), errCode(_errCode) {}

RetMSG::RetMSG(): msg("OK"), errCode(0) {}

bool RetMSG::isSuccess() {
    return errCode == 0;
}

const std::string &RetMSG::getMsg() {
    return msg;
}

int RetMSG::getErrCode() {
    return 0;
}

#endif //MHHSS_RET_MSG_H
