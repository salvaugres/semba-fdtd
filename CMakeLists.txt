cmake_minimum_required(VERSION 3.0)


ENABLE_LANGUAGE(Fortran)
project(semba-fdtd Fortran)

function(add_sources dir srcs)
    set(curdir ${CMAKE_CURRENT_LIST_DIR}/${dir})
    file(GLOB children RELATIVE ${curdir} ${curdir}/*)
    foreach(child ${children})
        if(IS_DIRECTORY ${curdir}/${child})
            if (dir STREQUAL ".")
                add_sources(${child} ${srcs})
            else()
                add_sources(${dir}/${child} ${srcs})
            endif()
        endif()
    endforeach()
    file(GLOB SRCS_LOCAL RELATIVE ${CMAKE_CURRENT_LIST_DIR} ${curdir}/*.F90)
    set(SRCS_LOCAL_ALL ${HDRS_LOCAL} ${TMPL_LOCAL} ${SRCS_LOCAL})
    set(${srcs} ${${srcs}} ${SRCS_LOCAL_ALL} PARENT_SCOPE)
    if (NOT dir STREQUAL ".")
        string(REPLACE "/" "\\\\" src_grp ${dir})
        source_group(${src_grp} FILES ${SRCS_LOCAL_ALL})
    endif()
endfunction()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

message(STATUS ${CMAKE_Fortran_COMPILER_ID})

add_library(semba-types 
	"src_main_pub/nfde_types.F90"
	"src_main_pub/fdetypes.F90"
	"src_main_pub/errorreport.F90"
)

option(CompileWithJSON "Compile JSON format parser" OFF)
if (CompileWithJSON)
	message(STATUS "Compiling JSON format parser")
	add_subdirectory(src_json_parser)
endif()

message(STATUS "Adding source files...")
add_sources(src_main_pub SRCS)
add_sources(src_wires_pub SRCS)

#add_executable(semba-fdtd ${SRCS})

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
	message(STATUS "Gfortran found...")
	set(CMAKE_Fortran_FLAGS "-ffree-form -ffree-line-length-0 -fdec -fcheck=bounds")
	add_definitions(-DCompileWithIncludeMpifh)
else()
	if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
		message(STATUS "INTEL FORTRAN found...")
		set(CMAKE_Fortran_FLAGS "-fpp -check all -debug full -traceback ")
	else()
		message(STATUS "Neither Intel nor Gfortran not found. Stopping.")
	endif()
endif()

option (CompileWithHDF "Use HDF" ON)
if (CompileWithHDF)
	message(STATUS "HDF libraries needed")
	add_definitions(-DCompileWithHDF)
	#HDF no funciona
	find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
	include_directories(${HDF5_INCLUDE_DIRS})
	target_link_libraries(semba-fdtd ${HDF5_LIBRARIES} ${HDF5_HL_LIBRARIES})
else()
	message(STATUS "HDF libraries NOT needed.")
endif()

option(CompileWithMPI "Use MPI" ON)
if (CompileWithMPI)
	message(STATUS "MPI libraries neeeded")
	add_definitions(-DCompileWithMPI)
	add_definitions(-DCompileWithGfortranMPIfix)
	message(STATUS "MPI: At your own risk open MPI may not currently work or even compile with Gfortran.")
	find_package(MPI REQUIRED COMPONENTS Fortran)
	add_definitions(${MPI_Fortran_COMPILE_OPTIONS})
	include_directories(${MPI_Fortran_INCLUDE_DIRS})
	link_directories(${MPI_Fortran_LIBRARIES})
	target_link_libraries(semba-fdtd ${MPI_Fortran_LIBRARIES})
	add_compile_options (${MPI_Fortran_COMPILER_FLAGS})
else()
	message(STATUS "MPI libraries NOT needed.")
endif()

add_definitions(		
-DCompileWithInt2 
-DCompileWithReal4 
-DCompileWithOpenMP 
-DCompileWithAnisotropic 
-DCompileWithEDispersives
-DCompileWithNF2FF
-DCompileWithNodalSources
-DCompileWithDMMA
-DCompileWithSGBC
-DCompileWithWires
)
