//
// Created by wujiahao on 18-7-13.
//

#include <gtest/gtest.h>

extern "C" void changestringint_(char *str, int *len, int *n);

TEST(easydhm_test, test1) {
   char str[5] = "abcd" ;
   int length = 5;
   std::cout << str << std::endl;
   changestringint_(str, &length, &length);
    std::cout << str << std::endl;
}