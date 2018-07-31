set(HEADER_FILES
        )

set(SOURCE_FILES
        main.cpp
        )

# set binary path and lib storage path for compiling process.
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)

add_executable(rdmg ${SOURCE_FILES} ${HEADER_FILES})

install(TARGETS rdmg
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        )