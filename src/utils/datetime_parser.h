//
// Created by wujiahao on 10/18/2018.
//

#ifndef PNOHS_ALPHA_DATETIME_PARSER_H
#define PNOHS_ALPHA_DATETIME_PARSER_H


#include <ctime>
#include <string>

class DatetimeParser {
public:
    static const std::string DEFAULT_DATETIME_FORMAT;
    /**
     *
     * @param nSrc
     * @param datetime_format
     * @return
     */
    static std::string getDateTimeStr(time_t nSrc, std::string datetime_format);

    /**
     *
     * @param n_starttime
     * @param timesteps
     * @param time_stride
     * @param datetime_format
     * @return
     */
    static std::string getDateTimeStr(time_t n_starttime, unsigned long timesteps, unsigned long time_stride, std::string datetime_format);

    /**
     *
     * @param s_starttime
     * @param timesteps
     * @param time_stride
     * @param datetime_format
     * @return
     */
    static std::string getDateTimeStr(std::string s_starttime, unsigned long timesteps, unsigned long time_stride, std::string datetime_format);

    /**
     *
     * @param sDatetime
     * @param datetime_format
     * @return
     */
    static time_t getStapeTimestamp(std::string sDatetime, std::string datetime_format);

    /**
     *
     * @param n_starttime
     * @param n_endtime
     * @param time_stride
     * @return
     */
    static unsigned long getTimesteps(time_t n_starttime, time_t n_endtime, unsigned long time_stride);

    /**
     *
     * @param n_starttime
     * @param timesteps
     * @param time_stride
     * @return
     */
    static unsigned long getTimesteps(time_t n_starttime, unsigned long timesteps, unsigned long time_stride);

    /**
     *
     * @param s_starttime
     * @param s_endtime
     * @param datetime_format
     * @param time_stride unit is hour
     * @return
     */
    static unsigned long getTimesteps(std::string s_starttime, std::string s_endtime, std::string datetime_format, unsigned long time_stride);

    /**
     *
     * @param n_starttime
     * @return
     */
    static unsigned getDayOfYear(time_t time);

    /**
     *
     * @param s_starttime
     * @param timesteps
     * @param time_stride
     * @param datetime_format
     * @return
     */
    static unsigned getDayOfYear(const std::string &s_starttime,
                                unsigned long timesteps,
                                unsigned long time_stride,
                                const std::string &datetime_format);
};


#endif //PNOHS_ALPHA_DATETIME_PARSER_H
