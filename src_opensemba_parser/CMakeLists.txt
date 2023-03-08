cmake_minimum_required(VERSION 3.0)

unset(SRCS)

project(opensemba-parser C CXX Fortran)
set(CMAKE_CXX_STANDARD 17)

if(NOT DEFINED CMAKE_TOOLCHAIN_FILE AND DEFINED ENV{CMAKE_TOOLCHAIN_FILE})
    set(CMAKE_TOOLCHAIN_FILE $ENV{CMAKE_TOOLCHAIN_FILE})
endif()

#add_subdirectory(src/)
add_subdirectory(external/opensemba-core)

message(STATUS "Creating build system for opensemba-parser with the following options:")
option(OPENSEMBA_PARSER_TESTS OFF)
if(OPENSEMBA_TESTS)
	enable_testing()
    add_subdirectory(test/)
endif()