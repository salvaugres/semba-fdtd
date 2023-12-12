cmake_minimum_required (VERSION 3.7)

project (mtlnsolver)
enable_language (Fortran)

message(STATUS "Creating build system for mtln solver")

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

include_directories(src/)

# add_subdirectory(external/lapack)

add_library(mtlnsolver
	"src/mtlnsolver.f90"
	"src/utils.f90"
)

# set(LAPACK_DIR ${CMAKE_CURRENT_SOURCE_DIR}/external/lapack/build/lib/Debug/lib/)
# message("lapack dir " ${LAPACK_DIR})
# find_package(BLAS)
# find_package(LAPACK)
# if(LAPACK_FOUND AND BLAS_FOUND)
#    set(lapackblas_libraries "${BLAS_LIBRARIES} ${LAPACK_LIBRARIES}")
# endif()

# message("LAPACK lib found " ${LAPACK_FOUND})
# message("BLAS lib found " ${BLAS_FOUND})

# target_link_directories(mtlnsolver PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/external/libs)
# target_link_libraries  (mtlnsolver lapack blas)
# target_link_libraries(mtlnsolver lapack blas)
# target_link_libraries(mtlnsolver ${CMAKE_CURRENT_SOURCE_DIR}/external/libs/LAPACK ${CMAKE_CURRENT_SOURCE_DIR}/external/libs/BLAS)

# add_subdirectory(test)

