cmake_minimum_required (VERSION 3.7)

project (mtlnsolver)
enable_language (Fortran)

message(STATUS "Creating build system for mtln solver")

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

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(LAPACK_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/linux-gcc-rls/lapack/")
	set(LAPACK_LIB ${LAPACK_DIR}liblapack.a)
	set(BLAS_LIB ${LAPACK_DIR}libblas.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(LAPACK_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/linux-intelLLVM-rls/lapack/")
	set(LAPACK_LIB ${LAPACK_DIR}liblapack.a)
	set(BLAS_LIB ${LAPACK_DIR}libblas.a)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	set(LAPACK_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/windows-intel-rls/lapack/")
	set(LAPACK_LIB ${LAPACK_DIR}liblapack.lib)
	set(BLAS_LIB ${LAPACK_DIR}libblas.lib)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	set(LAPACK_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/windows-intelLLVM-rls/lapack/")
	set(LAPACK_LIB ${LAPACK_DIR}liblapack.lib)
	set(BLAS_LIB ${LAPACK_DIR}libblas.lib)
else()
	message(FATAL_ERROR "lapack precompiled libraries for this platform and/or compiler have not been found.")
endif()
MESSAGE(STATUS "Using lapack precompiled libraries at: " ${LAPACK_DIR})

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(NGSPICE_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/linux-gcc-rls/ngspice/")
	set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(NGSPICE_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/linux-gcc-rls/ngspice/")
	set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.a)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	set(NGSPICE_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/windows-intel-rls/ngspice/")
	set(NGSPICE_LIB ${NGSPICE_DIR}ngspice.lib)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	set(NGSPICE_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/windows-intel-rls/ngspice/")
	set(NGSPICE_LIB ${NGSPICE_DIR}ngspice.lib)
else()
	message(FATAL_ERROR "ngspice precompiled libraries for this platform and/or compiler have not been found.")
endif()
MESSAGE(STATUS "Using ngspice precompiled libraries at: " ${NGSPICE_DIR})

target_link_libraries(mtlnsolver PRIVATE
	${LAPACK_LIB} 
	${BLAS_LIB} 
	${NGSPICE_LIB} 
	fhash
)