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
	"src/mtl.F90"
	"src/mtl_bundle.F90"
	"src/mtln_solver.F90"
	"src/network.F90"
	"src/network_bundle.F90"
	"src/utils.F90"
	"src/probes.F90"
	"src/types.F90"
	#"src/dispersive.F90"
	"src/rational_approximation.F90"
	# "src/ngspice_interface.F90"
)

# set(LAPACK_DIR "${CMAKE_SOURCE_DIR}/external/lapack/build/lib/")
# set(LAPACK_LIB ${LAPACK_DIR}liblapack.a)
# set(BLAS_LIB ${LAPACK_DIR}libblas.a)
set(NGSPICE_DIR "${CMAKE_SOURCE_DIR}/external/ngspice/src/.libs/")
set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.so)

target_link_libraries(mtlnsolver 
	# ${LAPACK_LIB} 
	${LAPACK_LIBRARIES}
	# ${BLAS_LIB} 
	# ${NGSPICE_LIB} 
	fhash
)

add_subdirectory(test)

