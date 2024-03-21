cmake_minimum_required (VERSION 3.18)

message(STATUS "Creating build system for test/system")

enable_language (CXX Fortran)
enable_testing ()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

add_library(
    system_test_fortran
    "test_map_wires_to_bundles.F90"
)

target_link_libraries(system_test_fortran
    mtlnsolver
    fhash
    ngspice_interface
    smbjson
    ${JSONFORTRAN_LIB}
)

add_executable(system_tests "main_test.cpp")

if (${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
    add_custom_command(TARGET system_tests POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${NGSPICE_DIR}ngspice.dll"      
            $<TARGET_FILE_DIR:system_tests>
    )
endif()


target_link_libraries(system_tests
    system_test_fortran
    GTest::gtest_main
)

add_test (
    NAME system_tests
    COMMAND system_tests
)

