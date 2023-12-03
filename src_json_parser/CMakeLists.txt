cmake_minimum_required (VERSION 3.18)

project (smbjson)

message(STATUS "Creating build system for smbjson")

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

add_subdirectory(external/fhash)
add_subdirectory(external/json-fortran)

add_library(smbjson 
	"src/smbjson.f90"
	"src/smbjson_labels.f90"
	"src/mesh.f90"
	"src/nfdetypes_extension.f90" 
)

target_link_libraries  (smbjson PRIVATE jsonfortran semba-types fhash)

add_subdirectory(test)
