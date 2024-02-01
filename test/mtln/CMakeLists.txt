cmake_minimum_required(VERSION 3.5)
enable_language (CXX Fortran)
enable_testing ()

message(STATUS "Creating build system for test/mtlnsolver")

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

add_library(
    mtln_solver_tests_fortran
    "testingTools.F90"
    "test_preprocess.F90"
    "test_spice.F90"
    "test_math.F90"
    "test_mtl.F90"
    "test_mtl_bundle.F90"
    "test_coaxial_line_paul_8_6_square.F90"
    "test_fhash.F90"
    "test_dispersive.F90"
    "test_networks.F90"
)
target_link_libraries(mtln_solver_tests_fortran
    mtlnsolver 
    fhash
)

add_executable(mtlnsolver_tests "main_test.cpp")

message(STATUS "Copying to: ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CONFIG}/ngspice.dll")
# configure_file(
#     "${NGSPICE_DIR}ngspice.dll"
#     "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CONFIG}/ngspice.dll"
#     COPYONLY
# )
message(STATUS "Copying to:" $<TARGET_FILE_DIR:mtlnsolver_tests>)
# configure_file(
#     "${NGSPICE_DIR}ngspice.dll"
#     "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${CONFIG}/ngspice.dll"
#     COPYONLY
# )
if (${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
    add_custom_command(TARGET mtlnsolver_tests POST_BUILD        # Adds a post-build event to MyTest
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${NGSPICE_DIR}ngspice.dll"      
            $<TARGET_FILE_DIR:mtlnsolver_tests>
    )
endif()

target_link_libraries(mtlnsolver_tests
    mtln_solver_tests_fortran
    GTest::gtest_main
)