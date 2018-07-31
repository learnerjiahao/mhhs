set(EASYDHM_SOURCE_FILES
        main.cpp
        )

# set binary path and lib storage path for compiling process.
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)

add_executable(easydhm-all ${EASYDHM_SOURCE_FILES})
target_link_libraries(easydhm-all ${EXTRA_LIBS} ${EASYDHMLIB})

install(TARGETS easydhm-all
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        )