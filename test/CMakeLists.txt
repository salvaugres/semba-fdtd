cmake_minimum_required (VERSION 3.18)

message(STATUS "Creating build system for fdtd_tests")

enable_language (CXX Fortran)
enable_testing ()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

add_subdirectory(mtln)
add_subdirectory(smbjson)
add_subdirectory(system)

add_executable(fdtd_tests 
    "fdtd_tests.cpp"
)

target_link_libraries(fdtd_tests
    mtln_tests
    smbjson_tests
    system_tests
    GTest::gtest_main
)

if (${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
    add_custom_command(TARGET fdtd_tests POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${NGSPICE_DIR}ngspice.dll"      
            $<TARGET_FILE_DIR:fdtd_tests>
    )
endif()