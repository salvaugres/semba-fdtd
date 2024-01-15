cmake_minimum_required (VERSION 3.7)

project (mtlnsolver)
enable_language (Fortran)

message(STATUS "Creating build system for mtln solver")

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

include_directories(src/)
add_subdirectory(external/fhash/)

add_library(mtlnsolver
	"src/mtl.f90"
	"src/mtl_bundle.f90"
	"src/mtln_solver.f90"
	"src/network.f90"
	"src/network_bundle.f90"
	"src/utils.f90"
	"src/probes.f90"
	"src/types.f90"
	"src/dispersive.f90"
	"src/rational_approximation.f90"
	"src/ngspice_interface.f90"
)

set(LAPACK_DIR "/mnt/c/users/alberto/Work/ugr-fdtd/fdtd/src_mtln/external/lapack/build/lib/")
set(LAPACK_LIB ${LAPACK_DIR}liblapack.a)
set(BLAS_LIB ${LAPACK_DIR}libblas.a)
set(NGSPICE_DIR "/mnt/c/users/alberto/Work/ugr-fdtd/fdtd/src_mtln/external/ngspice/src/.libs/")
set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.so)

target_link_libraries(mtlnsolver 
	${LAPACK_LIB} 
	${BLAS_LIB} 
	${NGSPICE_LIB} 
	fhash
)

add_subdirectory(test)

