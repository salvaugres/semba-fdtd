cmake_minimum_required(VERSION 3.0)

message(STATUS "Creating build system for json-parser")

project(opensemba-parser Fortran)
set(CMAKE_CXX_STANDARD 17)

enable_testing()
add_subdirectory(test/)
