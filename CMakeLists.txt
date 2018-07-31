cmake_minimum_required (VERSION 3.10)
set(CMAKE_CXX_STANDARD 11)

include(config.cmake)
project (${AppName})

add_subdirectory(src)
add_subdirectory(test)
add_subdirectory(tool)

#option(TEST_BUILD_ENABLE_FLAG "Enable test" ON) # enable test
#if (TEST_BUILD_ENABLE_FLAG STREQUAL "ON")
#    MESSAGE(STATUS "TEST is enabled")
#    add_subdirectory(test)
#endif ()
#
#option(TOOLS_BUILD_ENABLE_FLAG "Enable tools building" ON) # enable tool building (in tool directory) binary.(tool example: convert simulation result binary file to text file)
#if (TOOLS_BUILD_ENABLE_FLAG STREQUAL "ON")
#    add_subdirectory(tool)
#endif ()

