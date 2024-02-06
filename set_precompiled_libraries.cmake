if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(JSONFORTRAN_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/linux-gcc-rls/jsonfortran/")
	set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}libjsonfortran.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(JSONFORTRAN_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/linux-intel-rls/jsonfortran/")
	set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}libjsonfortran.a)
elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(JSONFORTRAN_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/linux-intelLLVM-rls/jsonfortran/")
	set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}libjsonfortran-static.a)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	set(JSONFORTRAN_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/windows-intel-rls/jsonfortran/")
	set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}jsonfortran.lib)
	elseif (${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	set(JSONFORTRAN_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/windows-intelLLVM-rls/jsonfortran/")
	set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}jsonfortran-static.lib)
else()
	message(FATAL_ERROR "json-fortran precompiled libraries for this platform and/or compiler have not been found.")
endif()
include_directories(${JSONFORTRAN_DIR}/include/)
MESSAGE(STATUS "Using json-fortran precompiled libraries at: " ${JSONFORTRAN_DIR})

if (CompileWithMTLN)
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


	if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux" AND ${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
		set(NGSPICE_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/linux-gcc-rls/ngspice/")
		set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.a)
	elseif (${CMAKE_SYSTEM_NAME} STREQUAL "Linux" AND ${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM" AND ${CMAKE_BUILD_TYPE} STREQUAL "Debug")
	set(NGSPICE_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/linux-intelLLVM-dbg/ngspice/")
		set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.a)
		elseif (${CMAKE_SYSTEM_NAME} STREQUAL "Linux" AND ${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM")
		set(NGSPICE_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/linux-intelLLVM-rls/ngspice/")
		set(NGSPICE_LIB ${NGSPICE_DIR}libngspice.a)
	elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Windows" AND 
		(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR ${CMAKE_Fortran_COMPILER_ID} STREQUAL "IntelLLVM"))
		set(NGSPICE_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/windows-intel-rls/ngspice/")
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
endif()