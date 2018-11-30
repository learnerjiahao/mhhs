# test
set(M_UINT_TEST_NAME "unit-tests")

enable_testing()

if (NOT (MPI_ENABLE_FLAG))
    set(TEST_MPI_ENABLE_FLAG OFF)  # if we disabled MPI_ENABLE_FLAG,we cann't also use TEST_MPI_ENABLE_FLAG.
endif ()
MESSAGE(STATUS "MPI in test: ${TEST_MPI_ENABLE_FLAG}")

if (NOT TARGET gtest)
    add_subdirectory(${PROJECT_SOURCE_DIR}/vendor/src/googletest/googletest ${PROJECT_BINARY_DIR}/vendor/googletest)
endif ()
include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

# Standard linking to gtest stuff.
set(EXTRA_LIBS ${M_LIB_NAME} gtest gtest_main ${EXTRA_LIBS})
MESSAGE(STATUS "Set googletest as test library.")

#add_definitions("-DNO_MPI")
add_subdirectory(unit)
