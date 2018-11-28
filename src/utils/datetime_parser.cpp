//
// Created by wujiahao on 10/18/2018.
//

#include <iomanip>
#include <sstream>
#include <cstring>
#include "datetime_parser.h"

const std::string DatetimeParser::DEFAULT_DATETIME_FORMAT = "%Y-%m-%d %H:%M";

std::string DatetimeParser::getDateTimeStr(time_t nSrc, std::string datetime_format) {
    char tmp_datatime[128]= {0};
    struct tm tmp_time;
    tmp_time = *localtime(&nSrc);
    strftime(tmp_datatime, 64, datetime_format.c_str(), &tmp_time);
    std::string sRetDataTime(tmp_datatime);
    return sRetDataTime;
}

time_t DatetimeParser::getStapeTimestamp(std::string sDatetime, std::string datetime_format) {
    int year, month, day, hour, minute;
    sscanf(sDatetime.c_str() , "%d-%d-%d %d:%d", &year, &month, &day, &hour, &minute);
    time_t rawTime;
    time(&rawTime);
    struct tm * parsedTime;
    parsedTime = localtime(&rawTime);
    // tm_year is years since 1900
    parsedTime->tm_year = year - 1900;
    // tm_months is months since january
    parsedTime->tm_mon = month - 1;
    parsedTime->tm_mday = day;
    parsedTime->tm_hour = hour;
    parsedTime->tm_min = minute;
    parsedTime->tm_sec = 0;
    return mktime(parsedTime);
}

unsigned long DatetimeParser::getTimesteps(time_t n_starttime, time_t n_endtime, unsigned long time_stride) {
    return (n_endtime - n_starttime) / (time_stride * 3600);
}

unsigned long DatetimeParser::getTimesteps(std::string s_starttime, std::string s_endtime, std::string datetime_format,
                                           unsigned long time_stride) {
    time_t n_starttime = getStapeTimestamp(s_starttime, datetime_format);
    time_t n_endtime = getStapeTimestamp(s_endtime, datetime_format);
    return (n_endtime - n_starttime) / (time_stride * 3600);
}

unsigned long DatetimeParser::getTimesteps(time_t n_starttime, unsigned long timesteps, unsigned long time_stride) {
    return n_starttime + time_stride * 3600 * timesteps;
}

std::string DatetimeParser::getDateTimeStr(time_t n_starttime, unsigned long timesteps, unsigned long time_stride,
                                           std::string datetime_format) {
    return DatetimeParser::getDateTimeStr(DatetimeParser::getTimesteps(n_starttime, timesteps, time_stride), datetime_format);
}

std::string DatetimeParser::getDateTimeStr(std::string s_starttime, unsigned long timesteps, unsigned long time_stride,
                                           std::string datetime_format) {
    return DatetimeParser::getDateTimeStr(DatetimeParser::getTimesteps(DatetimeParser::getStapeTimestamp(s_starttime, datetime_format),
            timesteps, time_stride),
                    datetime_format);
}
