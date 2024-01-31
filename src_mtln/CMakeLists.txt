cmake_minimum_required (VERSION 3.7)

project (mtlnsolver)
enable_language (Fortran)

message(STATUS "Creating build system for mtlnsolver")

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

add_library(mtlnsolver
	"mtl.F90"
	"mtl_bundle.F90"
	"mtln_solver.F90"
	"network.F90"
	"network_bundle.F90"
	"utils.F90"
	"probes.F90"
	"types.F90"
	"dispersive.F90"
	"rational_approximation.F90"
	"circuit.F90"
	"ngspice_interface.F90"
	"mtln_types.F90"
	"preprocess.F90"
)

target_link_libraries(mtlnsolver PRIVATE
	${LAPACK_LIB} 
	${BLAS_LIB} 
	ngspice
	fhash
)