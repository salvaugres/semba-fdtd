cmake_minimum_required (VERSION 3.5)

project (smbjson)

message(STATUS "Creating build system for smbjson")

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

add_library(smbjson 
	"src/smbjson.f90"
	"src/nfdetypes_extension.f90" 
)

include_directories    (smbjson PRIVATE ${jsonfortran_INCLUDE_DIRS})
target_link_directories(smbjson PRIVATE ${jsonfortran_LIBRARY_DIRS})
target_link_libraries  (smbjson jsonfortran semba-types)

add_subdirectory(test)
