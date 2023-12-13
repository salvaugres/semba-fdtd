cmake_minimum_required (VERSION 3.7)

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
	"src/domain.f90"
	"src/mtln.f90"
	"src/networks.f90"
	"src/networks_bundles.f90"
	"src/utils.f90"
)

set(LAPACK_DIR "/mnt/c/users/alberto/Work/ugr-fdtd/fdtd/src_mtln/external/lapack/build/lib/")
set(LAPACK_LIB ${LAPACK_DIR}liblapack.a)
set(BLAS_LIB ${LAPACK_DIR}libblas.a)

target_link_libraries(mtlnsolver ${LAPACK_LIB} ${BLAS_LIB})

add_subdirectory(test)

