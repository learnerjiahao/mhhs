# store temp file, e.g. files for IO test.
set(TEST_TEMP_STORAGE_PATH ${CMAKE_CURRENT_BINARY_DIR})

include_directories(${PROJECT_SOURCE_DIR}/vendor/include)
include_directories(${PROJECT_SOURCE_DIR}/src)

set(TEST_HEADER_FILES)

set(TEST_SOURCE_FILES
#        model/hbv_test.cpp
#        readinputs/readinputs_test.cpp
#        utils/mpiutil_test.cpp
#        simulation/simulation.cpp
#        subbasin/subbasin_test.cpp
#        model/xinanjiang_test.cpp
        model/easydhm_test.cpp)

# set binary path and lib storage path wile compiling process.
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)

add_executable(${M_UINT_TEST_NAME} ${TEST_SOURCE_FILES} ${TEST_HEADER_FILES})

# Extra linking for the project.
#set(EXTRA_LIBS ${M_LIB_NAME} ${EXTRA_LIBS})
# Extra linking for the project.
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -rpath -O3 -ipo -xavx")
target_link_libraries(${M_UINT_TEST_NAME} ${EXTRA_LIBS} ${EASYDHMLIB})

# This is so you can do 'make test' to see all your tests run, instead of
# manually running the executable runUnitTests to see those specific tests.
add_test(NAME exampleTests COMMAND ${M_UINT_TEST_NAME})


##########################
##### install files
##########################
install(TARGETS ${M_UINT_TEST_NAME}
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        )
