set(AppName mhhss)
set(M_EXECUTE_BIN_NAME ${AppName})
set(M_LIB_NAME "mhhsslib")

enable_language(CXX)
enable_language(C)

add_definitions("-Wall -g")
option(USE_PTHREAD "Use pthread" OFF) #

option(DEVE_MODE "Enable development mode" OFF)

option(USE_NOBLOCKING_SEND "Use MPI_Isend to send datas in master thread" OFF)
option(USE_HNOBLOCKING_SEND "Use half MPI_Isend to send datas in master thread" OFF)

option(USE_HNOBLOCKING_RECV "Use half MPI_Irecv to recv datas in slave thread" OFF)

find_package(MPI REQUIRED)
MESSAGE(STATUS "MPI_INCLUDE dir:" ${MPI_CXX_INCLUDE_PATH})
MESSAGE(STATUS "MPI_LIBRARIES dir:" ${MPI_CXX_LIBRARIES})
set(COMPILE_FLAGS "${COMPILE_FLAGS} ${MPI_CXX_COMPILE_FLAGS}")
set(LINK_FLAGS "${LINK_FLAGS} ${MPI_CXX_LINK_FLAGS}")
include_directories(${MPI_CXX_INCLUDE_PATH})
set(EXTRA_LIBS ${EXTRA_LIBS} ${MPI_CXX_LIBRARIES}) #add mpi lib

add_definitions("-DUSE_MPI")

# The version number.
set (Tutorial_VERSION_MAJOR 1)
set (Tutorial_VERSION_MINOR 0)

if(DEVE_MODE STREQUAL "ON")
    message(STATUS "DEVE_MODE is ON")
    add_definitions("-Wall -g")
    #dbug mode to develope
    add_definitions("-DDEV_MODE")
else()
    message(STATUS "DEVE_MODE is OFF")
endif()

if(USE_PTHREAD STREQUAL "ON")
    find_package (Threads REQUIRED)
    set(EXTRA_LIBS ${EXTRA_LIBS} ${CMAKE_THREAD_LIBS_INIT})
    add_definitions("-DUSE_PTHREAD")
    MESSAGE(STATUS "Pthread is used.")
    if(USE_NOBLOCKING_SEND STREQUAL "ON")
        message(STATUS "Use MPI_Isend in master thread")
        add_definitions("-DUSE_NOBLOCKING_SEND")
    else()
        if(USE_HNOBLOCKING_SEND STREQUAL "ON")
            message(STATUS "Use half MPI_Isend in master thread")
            add_definitions("-DUSE_HNOBLOCKING_SEND")
        else()
            message(STATUS "Use MPI_Send in master thread")
        endif()
    endif()

    if(USE_HNOBLOCKING_RECV STREQUAL "ON")
        message(STATUS "Use half MPI_Irecv in slave thread")
        #use MPI_Isend or MPI_Send in main thread
        add_definitions("-DUSE_HNOBLOCKING_RECV")
    else()
        message(STATUS "Use MPI_Recv in slave thread")
    endif()
else()
    MESSAGE(STATUS "Not use pthread.")
endif()

