cmake_minimum_required (VERSION 3.18)

ENABLE_LANGUAGE(Fortran)
project(semba-fdtd Fortran)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

message(STATUS ${CMAKE_Fortran_COMPILER_ID})

option(CompileWithHDF "Use HDF" OFF)
option(CompileWithMPI "Use MPI" OFF)
option(CompileExecutable "Compiles semba-fdtd executable" ON)
option(CompileWithMTLN "Use MTLN modules" ON)

include("${CMAKE_CURRENT_SOURCE_DIR}/set_precompiled_libraries.cmake")

add_library(semba-types 
	"src_main_pub/nfde_types.F90"
	"src_main_pub/fdetypes.F90"
	"src_mtln/mtln_types.F90"
)
add_subdirectory(external/fhash/)
add_subdirectory(external/googletest/)

add_subdirectory(src_json_parser)
add_subdirectory(test/smbjson)

add_subdirectory(test/system)

if (CompileWithMTLN)
	add_subdirectory(src_mtln)
	add_subdirectory(test/mtln)
endif()

if (CompileExecutable)
	add_executable(semba-fdtd 
		"src_main_pub/errorreport.F90"
		"src_main_pub/anisotropic.F90"
		"src_main_pub/borderscpml.F90"
		"src_main_pub/bordersmur.F90"
		"src_main_pub/bordersother.F90"
		"src_main_pub/calc_constants.F90"
		"src_main_pub/dmma_thin_slot.F90"
		"src_main_pub/electricdispersive.F90"
		"src_main_pub/EpsMuTimeScale.F90"
		"src_main_pub/farfield.F90"
		"src_main_pub/getargs.F90"
		"src_main_pub/healer.F90"
		"src_main_pub/lumped_types.F90"
		"src_main_pub/lumped.F90"
		"src_main_pub/magneticdispersive.F90"
		"src_main_pub/maloney_nostoch.F90"
		"src_main_pub/mpicomm.F90"
		"src_main_pub/nodalsources.F90"
		"src_main_pub/observation.F90"
		"src_main_pub/planewaves.F90"
		"src_main_pub/pml_bodies.F90"
		"src_main_pub/postprocess.F90"
		"src_main_pub/preprocess.F90"
		"src_main_pub/resuming.F90"
		"src_main_pub/interpreta_switches.F90"
		"src_main_pub/semba_fdtd.F90"
		"src_main_pub/storegeom.F90"
		"src_main_pub/timestepping.F90"
		"src_main_pub/version.F90"
		"src_wires_pub/wires_types.F90"
		"src_wires_pub/wires.F90"
	)
	target_link_libraries(semba-fdtd semba-types smbjson)
	include_directories(${CMAKE_BINARY_DIR}/mod)
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
	message(STATUS "Gfortran found...")
	set(CMAKE_Fortran_FLAGS "-ffree-form -ffree-line-length-none -fdec -fcheck=bounds")
	add_definitions(-DCompileWithIncludeMpifh)
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
	message(STATUS "INTEL FORTRAN found...")
	set(CMAKE_Fortran_FLAGS "-fpp -check all -debug full -traceback ")
endif()

if (CompileWithHDF)
	message(STATUS "HDF libraries needed")
	add_definitions(-DCompileWithHDF)
	find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
	include_directories(${HDF5_INCLUDE_DIRS})
	target_link_libraries(semba-fdtd ${HDF5_LIBRARIES} ${HDF5_HL_LIBRARIES})
else()
	message(STATUS "HDF libraries NOT needed.")
endif()

if (CompileWithMPI)
	message(STATUS "MPI libraries needed")
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


