set(PRECOMPILED_DIR "${PROJECT_SOURCE_DIR}/precompiled_libraries")

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(JSONFORTRAN_DIR "${PRECOMPILED_DIR}/linux-gcc-rls/jsonfortran/")
	set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}libjsonfortran.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "NVHPC" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
		set(JSONFORTRAN_DIR "${PRECOMPILED_DIR}/linux-nvhpc-rls/jsonfortran/")
		set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}libjsonfortran.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(JSONFORTRAN_DIR "${PRECOMPILED_DIR}/linux-intel-rls/jsonfortran/")
	set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}libjsonfortran.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(JSONFORTRAN_DIR "${PRECOMPILED_DIR}/linux-intelLLVM-rls/jsonfortran/")
	set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}libjsonfortran-static.a)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	set(JSONFORTRAN_DIR "${PRECOMPILED_DIR}/windows-intel-rls/jsonfortran/")
	set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}jsonfortran.lib)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	set(JSONFORTRAN_DIR "${PRECOMPILED_DIR}/windows-intelLLVM-rls/jsonfortran/")
	set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}jsonfortran-static.lib)
else()
	message(FATAL_ERROR "json-fortran precompiled libraries for this platform and/or compiler have not been found.")
endif()
include_directories(${JSONFORTRAN_DIR}/include/)
MESSAGE(STATUS "Using json-fortran precompiled libraries at: " ${JSONFORTRAN_DIR})

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(LAPACK_DIR "${PRECOMPILED_DIR}/linux-gcc-rls/lapack/")
	set(LAPACK_LIB ${LAPACK_DIR}liblapack.a)
	set(BLAS_LIB ${LAPACK_DIR}libblas.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "NVHPC" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(LAPACK_DIR "/opt/nvidia/hpc_sdk/Linux_x86_64/24.3/compilers/lib/")
	set(LAPACK_LIB ${LAPACK_DIR}liblapack.a)
	set(BLAS_LIB ${LAPACK_DIR}libblas.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(LAPACK_DIR "${PRECOMPILED_DIR}/linux-intel-rls/lapack/")
	set(LAPACK_LIB ${LAPACK_DIR}liblapack.a)
	set(BLAS_LIB ${LAPACK_DIR}libblas.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(LAPACK_DIR "${PRECOMPILED_DIR}/linux-intelLLVM-rls/lapack/")
	set(LAPACK_LIB ${LAPACK_DIR}liblapack.a)
	set(BLAS_LIB ${LAPACK_DIR}libblas.a)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	set(LAPACK_DIR "${PRECOMPILED_DIR}/windows-intel-rls/lapack/")
	set(LAPACK_LIB ${LAPACK_DIR}liblapack.lib)
	set(BLAS_LIB ${LAPACK_DIR}libblas.lib)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	set(LAPACK_DIR "${PRECOMPILED_DIR}/windows-intelLLVM-rls/lapack/")
	set(LAPACK_LIB ${LAPACK_DIR}liblapack.lib)
	set(BLAS_LIB ${LAPACK_DIR}libblas.lib)
else()
	message(FATAL_ERROR "lapack precompiled libraries for this platform and/or compiler have not been found.")
endif()
MESSAGE(STATUS "Using lapack precompiled libraries at: " ${LAPACK_DIR})

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(NGSPICE_DIR "${PRECOMPILED_DIR}/linux-gcc-rls/ngspice/")
	set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "NVHPC" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(NGSPICE_DIR "${PRECOMPILED_DIR}/linux-gcc-rls/ngspice/")
	set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(NGSPICE_DIR "${PRECOMPILED_DIR}/linux-intel-dbg/ngspice/")
	set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux" AND ${CMAKE_BUILD_TYPE} STREQUAL "Debug")
	set(NGSPICE_DIR "${PRECOMPILED_DIR}/linux-intelLLVM-dbg/ngspice/")
	set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(NGSPICE_DIR "${PRECOMPILED_DIR}/linux-intelLLVM-rls/ngspice/")
	set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.a)
elseif ((${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR ${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM") AND 
		${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	set(NGSPICE_DIR "${PRECOMPILED_DIR}/windows-intel-rls/ngspice/")
	set(NGSPICE_LIB ${NGSPICE_DIR}ngspice.lib)
	add_library(ngspice SHARED IMPORTED)
	set_target_properties(ngspice PROPERTIES 
		IMPORTED_LOCATION "${NGSPICE_DIR}ngspice.dll"
		IMPORTED_IMPLIB "${NGSPICE_DIR}ngspice.lib"
	)
else()
	message(FATAL_ERROR "ngspice precompiled libraries for this platform and/or compiler have not been found.")
endif()

MESSAGE(STATUS "Using ngspice precompiled libraries at: " ${NGSPICE_DIR})

if (CompileWithHDF AND NOT HDF5_FOUND)
	add_definitions(-DCompileWithHDF)
	if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
		if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" OR ${CMAKE_Fortran_COMPILER_ID} STREQUAL "NVHPC")
			# System libraries. Install with sudo apt install libhdf5	    
		elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR ${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM")
			set(HDF5_DIR ${PRECOMPILED_DIR}/linux-intel-rls/hdf5/cmake/)
		endif()
	elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
		if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR ${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM")
			set(HDF5_DIR ${PRECOMPILED_DIR}/windows-intel-rls/hdf5/cmake/)
		endif()
	endif()
	
	find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
	if (HDF5_FOUND)
		message(STATUS "hdf5 found at: ${HDF5_Fortran_INCLUDE_DIRS}")
	else()
		message(FATAL_ERROR "hdf5 precompiled libraries for this platform and/or compiler have not been found.")
	endif()
endif()

if (CompileWithMPI)
	add_definitions(-DCompileWithMPI)
	find_package(MPI REQUIRED COMPONENTS Fortran)
	if (MPI_FOUND)
		message(STATUS "MPI found at: ${MPI_Fortran_INCLUDE_DIRS}")
	endif()

	add_definitions(${MPI_Fortran_COMPILE_OPTIONS})
	include_directories(${MPI_Fortran_INCLUDE_DIRS})
	link_directories(${MPI_Fortran_LIBRARIES})
	add_compile_options (${MPI_Fortran_COMPILER_FLAGS})
endif()
