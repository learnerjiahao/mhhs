//
// Created by parallels on 11/28/18.
//

#include "ret_msg.h"
#include <string>

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
