cmake_minimum_required (VERSION 3.18)

project (smbjson)

message(STATUS "Creating build system for smbjson project")

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
	set(JSONFORTRAN_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/linux-gcc-rls/jsonfortran/")
	set(JSONFORTRAN_LIB ${JSONFORTRAN_DIR}libjsonfortran.a)
    include_directories(${JSONFORTRAN_DIR}/include/)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND ${CMAKE_SYSTEM_NAME} STREQUAL "Windows")
	# set(NGSPICE_DIR "${CMAKE_SOURCE_DIR}/precompiled_libraries/windows-intel-rls/ngspice/")
	# set(NGSPICE_LIB ${NGSPICE_DIR}ngspice.lib)
endif()

add_subdirectory(src)
add_subdirectory(test)
