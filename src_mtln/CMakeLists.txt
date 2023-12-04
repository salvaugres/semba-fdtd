cmake_minimum_required (VERSION 3.5)

project (mtlnsolver)
enable_language (Fortran)

message(STATUS "Creating build system for mtln solver")

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

include_directories(src/)

add_library(mtlnsolver
	"src/mtlnsolver.f90"
)

# message(STATUS "json-fortran include dirs: " ${jsonfortran_INCLUDE_DIRS})
# message(STATUS "json-fortran lib dirs: " ${jsonfortran_LIBRARY_DIRS})
# include_directories    (mtln_fortran PRIVATE ${jsonfortran_INCLUDE_DIRS})
# target_link_directories(mtln_fortran PRIVATE ${jsonfortran_LIBRARY_DIRS})
# target_link_libraries  (mtlnsolver PRIVATE semba-types)

add_subdirectory(test)
