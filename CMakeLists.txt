cmake_minimum_required(VERSION 3.0)


option (CompileWithGNU "Use GNU Fortran" OFF)
option (CompileWithIntelWindows "Use Intel Windows Fortran" OFF)
option (CompileWithIntelLinux "Use Linux Windows Fortran" OFF)
option (CompileWithNvidiaWSL "Use Nvidia Fortran under WSL" ON)
option (CompileWithNvidiaCINECA "Use Nvidia Fortran under CINECA" ON)
option (CompileWithHDF "Use HDF" OFF)
option (CompileWithMPI "Use MPI" OFF)
option (CompileWithConformal "Use CompileWithConformal" OFF)
option (CompileWithIncludeMpifh "Use CompileWithIncludeMpifh" OFF)
option (CompileWithGamusino "Use CompileWithGamusino" OFF)

ENABLE_LANGUAGE(Fortran)
project(semba-fdtd Fortran)

message(${CMAKE_Fortran_COMPILER_ID} "=CMAKE_Fortran_COMPILER_ID")
message(${CMAKE_Fortran_COMPILER}    "=CMAKE_Fortran_COMPILER   ")

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

message(STATUS ${CMAKE_Fortran_COMPILER_ID})

message(STATUS "Adding source files...")
add_sources(. SRCS)
message(STATUS ${SRCS})

add_executable(semba-fdtd ${SRCS})

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
endif()


if (CompileWithIntelWindows)
	if(CMAKE_Fortran_COMPILER_ID MATCHES "IntelLLVM")
        message(${CMAKE_Fortran_COMPILER_ID} "=CMAKE_Fortran_COMPILER_ID")
        message(${CMAKE_Fortran_COMPILER}    "=CMAKE_Fortran_COMPILER   ")
		message(STATUS "IntelLLVM WINDOWS found...")
		set(CMAKE_Fortran_FLAGS_RELEASE "/nologo /MP  /free /Qopenmp \
		/warn:declarations /warn:ignore_loc /warn:truncated_source \
		/warn:uncalled /warn:shape /assume:noprotect_constants /fpe:0 /fp:fast=2 \
		/fpconstant /traceback /check:all /libs:static /threads /dbglibs /c /STACK:10000000,10000000")
		set(CMAKE_Fortran_FLAGS_DEBUG "/nologo /debug:full /MP /Od /fpp /free /Qopenmp /warn:all \
		/assume:noprotect_constants /fpe:0 /fp:strict /fpconstant /Qcomplex-limited-range \
		/Qfp-stack-check /traceback /check:all /libs:static /threads /dbglibs /c \
		/MAP /MAPINFO:EXPORTS /ASSEMBLYDEBUG /SUBSYSTEM:CONSOLE /STACK:1000000000,1000000000")
	else()
		message(STATUS "IntelLLVM not found.")
		#exit()
	endif()
endif()


if (CompileWithIncludeMpifh)
	add_definitions(-DCompileWithIncludeMpifh)
endif()

if (CompileWithGamusino)
	add_definitions(-DCompileWithGamusino)
endif()
		
		
if (CompileWithIntelLinux)
    message(STATUS "CompileWithIntelLinux requested")
	if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
        message(${CMAKE_Fortran_COMPILER_ID} "=CMAKE_Fortran_COMPILER_ID")
        message(${CMAKE_Fortran_COMPILER}    "=CMAKE_Fortran_COMPILER   ")
		message(STATUS "Intel LINUX found...")
	    add_definitions(-DCompileWithIncludeMpifh)
		add_definitions(-DCompileWithGamusino)
#Gamusino se precisa en algunos compiladores Intel. Quita algunos flags de entrada pero casi no tiene ningun impacto mas alla de mi debugeo		
		set(CMAKE_Fortran_FLAGS_RELEASE "-traceback -parallel -O3 -march=native -qopt-prefetch=3 \
										-ipo -qopt-matmul -fpp -qopenmp -warn declarations \
										-warn ignore_loc -warn truncated_source -warn uncalled -warn shape -fp-model fast=2 \
										-static-intel -threads -heap-arrays 10000000")
		set(CMAKE_Fortran_FLAGS_DEBUG "-g -traceback -parallel -fpp -free -qopenmp -warn all -warn nounused \
									-diag-disable=8889 -march=native \
									-assume	noprotect_constants \
									-fpe:0 -fp:strict -fpconstant -complex-limited-range -fp-stack-check \
									-check all -static-intel -threads -dbglibs -heap-arrays 1000000000")
	else()
		message(STATUS "Intel LINUX not found.")
	#exit()
	endif()
endif()



if (CompileWithGNU)
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
        message(${CMAKE_Fortran_COMPILER_ID} "=CMAKE_Fortran_COMPILER_ID")
        message(${CMAKE_Fortran_COMPILER}    "=CMAKE_Fortran_COMPILER   ")
	message(STATUS "Gfortran found...")
#		set(CMAKE_Fortran_FLAGS_RELEASE "-Wimplicit-interface  -Wuse-without-only   -Wsurprising  -Wconversion   -Wunused  -Warray-temporaries" )
		set(CMAKE_Fortran_FLAGS_RELEASE "\
		-march=native \
        -ffree-form -ffree-line-length-none -fdec -fbacktrace -fopenmp -O3 -ffpe-trap=zero \
		-fprefetch-loop-arrays -flto -Wno-lto-type-mismatch \
        -funroll-loops  -cpp    -fopenmp  -fno-strict-aliasing  -ffast-math  -pthread ")
		set(CMAKE_Fortran_FLAGS_DEBUG "\
		-march=native \
        -ffree-form -ffree-line-length-none -fdec -fbacktrace -fopenmp -g  -ffpe-trap=zero \
		-fprefetch-loop-arrays -flto -Wno-lto-type-mismatch \
        -funroll-loops  -cpp    -fopenmp  -fno-strict-aliasing  -ffast-math  -pthread ")
		if (CompileWithMPI)
			add_definitions(-DCompileWithGfortranMPIfix)
			message(STATUS "MPI: At your own risk open MPI may not currently work or even compile with Gfortran.")
		endif()
	else()
		message(STATUS "Gfortran not found.")
		#exit()
	endif()
endif()


if (CompileWithNvidiaWSL)
	if(CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC")
		message(STATUS "NVHPC FOUND!!!")
        message(${CMAKE_Fortran_COMPILER_ID} "=CMAKE_Fortran_COMPILER_ID")
        message(${CMAKE_Fortran_COMPILER}    "=CMAKE_Fortran_COMPILER   ")
#		set(CMAKE_Fortran_FLAGS_RELEASE "-Wimplicit-interface  -Wuse-without-only   -Wsurprising  -Wconversion   -Wunused  -Warray-temporaries" )
#		-acc=multicore !para cpu \
#
        set(CMAKE_CUDA_ARCHITECTURES 86)
        set(CMAKE_CUDA_LINK_EXECUTABLE "<CMAKE_CXX_COMPILER> <LINK_FLAGS> <OBJECTS> -o <TARGET>")
        #enable_language(CUDA)
        set(NVC_GPU_LIST ${CMAKE_CUDA_ARCHITECTURES})
        list(TRANSFORM NVC_GPU_LIST PREPEND "cc")
        list(JOIN NVC_GPU_LIST "," NVC_GPU_LIST)
        message("NVC_GPU_LIST=" ${NVC_GPU_LIST} )

		set(CMAKE_Fortran_FLAGS_RELEASE "\
		-O1 -Mnoopenmp -acc=gpu -gpu=${NVC_GPU_LIST}  -Minfo=accel    \
        -cpp -pthread -Minline -L /opt/nvidia/hpc_sdk/Linux_x86_64/23.11/cuda/lib64/ -lnvhpcwrapnvtx")
		set(CMAKE_Fortran_FLAGS_DEBUG "\
		-g   -mp -Mnoopenmp -acc=gpu -gpu=${NVC_GPU_LIST}  -Minfo=accel \
        -cpp -pthread -Minline -L /opt/nvidia/hpc_sdk/Linux_x86_64/23.11/cuda/lib64/ -lnvhpcwrapnvtx")
		if (CompileWithMPI)
			add_definitions(-DCompileWithGfortranMPIfix)
			message(STATUS "MPI: At your own risk open MPI may not currently work or even compile with NVIDIA FORTRAN.")
		endif()
else()
		message(STATUS "NVHPC not found.")
		#exit()
	endif()
endif()



if (CompileWithNvidiaCINECA)
	if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
        message(${CMAKE_Fortran_COMPILER_ID} "=CMAKE_Fortran_COMPILER_ID")
        message(${CMAKE_Fortran_COMPILER}    "=CMAKE_Fortran_COMPILER   ")
		message(STATUS "PGI fotran found...")
#		set(CMAKE_Fortran_FLAGS_RELEASE "-Wimplicit-interface  -Wuse-without-only   -Wsurprising  -Wconversion   -Wunused  -Warray-temporaries" )
#		-acc=multicore !para cpu \
		set(CMAKE_Fortran_FLAGS_RELEASE "\
		-O3  -mp -cudalib -ta=tesla:managed -Minfo=accel   \
        -cpp -pthread ")
		set(CMAKE_Fortran_FLAGS_DEBUG "\
		-acc=gpu -gpu=cuda12.1 \
		-g -mp \
		-cpp  -pthread ")
		if (CompileWithMPI)
			add_definitions(-DCompileWithGfortranMPIfix)
			message(STATUS "MPI: At your own risk open MPI may not currently work or even compile with NVIDIA FORTRAN.")
	endif()

	else()
		message(STATUS "Gfortran not found.")
		#exit()
	endif()
endif()

option (CompileWithHDF "Use HDF" ON)
if (CompileWithHDF)
	message(STATUS "HDF libraries needed")
	add_definitions(-DCompileWithHDF)
	#HDF no funciona
	find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
	include_directories(${HDF5_INCLUDE_DIRS})
	target_link_libraries(semba-fdtd ${HDF5_LIBRARIES})
	message(STATUS "HDF libraries used")
	add_definitions(-DCompileWithHDF)
else()
	message(STATUS "HDF libraries NOT needed.")
endif()

option(CompileWithMPI "Use MPI" ON)
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
	message(STATUS "MPI libraries used")
else()
	message(STATUS "MPI libraries NOT used.")
endif()

if (CompileWithConformal)
	message(STATUS "Compiling with CompileWithConformal")
	add_definitions(-DCompileWithConformal)
endif()

add_definitions(		
-DCompileWithInt2 
-DCompileWithReal4 
-DxxCompileWithOpenMP 
-DCompileWithACC
-DCompileWithProfiling
-DCompileWithAnisotropic 
-DCompileWithEDispersives
-DCompileWithNF2FF
-DCompileWithNodalSources
-DCompileWithDMMA
-DCompileWithSGBC
-DCompileWithWires
 )


