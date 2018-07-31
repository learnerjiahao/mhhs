add_subdirectory(models/easydhm)
# include directory
include_directories(${PROJECT_SOURCE_DIR}/vendor/include)
set(HEADER_FILES
        utils/predefine.h
        utils/const.h
        subbasin/subbasin.h
        subbasin/hru.h
        subbasin/simulation_subbasin.h
        models/simple_hbv_pfmodel.h
        simulation/routing_data_meta.h
        simulation/trouting_data.h
        simulation/routed_data.h
        simulation/simulation.h
        readInputs/config.h
        context.h
        utils/mpiutil.h
        readInputs/parse_dispatch.h
        message/routed_data_imessage.h
        message/routed_data_imsgs_pool.h
        simulation/subbasins_container.h
        models/xinanjiang3_model.h
        models/base_model.h
        models/XinanjiangModel.h
        models/xinanjiang2_model.h)

set(SOURCE_FILES
        subbasin/subbasin.cpp
        subbasin/hru.cpp
        subbasin/simulation_subbasin.cpp
        models/simple_hbv_pfmodel.cpp
        simulation/routing_data_meta.cpp
        simulation/trouting_data.cpp
        simulation/routed_data.cpp
        simulation/simulation.cpp
        readInputs/config.cpp
        context.cpp
        readInputs/parse_dispatch.cpp
        utils/mpiutil.cpp
        message/routed_data_imessage.cpp
        message/routed_data_imsgs_pool.cpp
        simulation/subbasins_container.cpp
        models/xinanjiang3_model.cpp
        models/base_model.cpp
        models/XinanjiangModel.cpp
        models/xinanjiang2_model.cpp)

set(MAIN_FILE main.cpp)


# set binary path and lib storage path for compiling process.
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)

add_executable(${M_EXECUTE_BIN_NAME} ${MAIN_FILE})

add_library(${M_LIB_NAME} STATIC ${HEADER_FILES} ${SOURCE_FILES})

set(EXTRA_LIBS ${M_LIB_NAME} ${EXTRA_LIBS})

target_link_libraries(${M_EXECUTE_BIN_NAME} ${EXTRA_LIBS}) # extra lib

##########################
##### install files
##########################
install(TARGETS ${M_EXECUTE_BIN_NAME} ${M_LIB_NAME}
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        )

install(FILES ${HEADER_FILES} DESTINATION include) # todo head file