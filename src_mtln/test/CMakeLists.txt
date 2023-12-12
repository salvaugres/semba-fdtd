cmake_minimum_required(VERSION 3.5)
enable_language (C Fortran)
enable_testing ()

function (mangle_fortran_name CNAME FNAME)
    set (TMP)
    if (WIN32)
        string (TOUPPER "${FNAME}" TMP)
    else ()
        string (TOLOWER "${FNAME}_" TMP)
    endif ()
    set (${CNAME} ${TMP} PARENT_SCOPE)
endfunction ()

function (mangle_fortran_filename_list MANGLED)
    set (TMP)
    foreach (TFILE ${ARGN})
        string (REGEX REPLACE ".f90$" "" TESTNAME ${TFILE})
        mangle_fortran_name (C_TESTNAME ${TESTNAME})
        list (APPEND TMP ${C_TESTNAME})
    endforeach ()
    set (${MANGLED} ${TMP} PARENT_SCOPE)
endfunction()

function (add_fortran_test_executable TARGET)
    set (TEST_FILES ${ARGN})
    mangle_fortran_filename_list (TEST_FILES_MANGLED ${TEST_FILES})

    create_test_sourcelist (_ main.c ${TEST_FILES_MANGLED})

    add_library (${TARGET}_fortran ${TEST_FILES} "testingTools.f90")

    # target_link_directories(${TARGET}_fortran PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../external/lapack)
    # target_link_libraries (${TARGET}_fortran mtlnsolver lapack blas)
    
    add_executable (${TARGET} main.c)
    target_link_libraries (${TARGET} ${TARGET}_fortran)


    set (INDEX 0)
    list (LENGTH TEST_FILES LEN)
    while (${LEN} GREATER ${INDEX})
        list (GET TEST_FILES ${INDEX} TEST_FILE)
        list (GET TEST_FILES_MANGLED ${INDEX} TEST_FILE_MANGLED)
        add_test (
            NAME ${TEST_FILE}
            COMMAND $<TARGET_FILE:${TARGET}> ${TEST_FILE_MANGLED}
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/src_mtln/test/
        )
        math (EXPR INDEX "${INDEX} + 1")
    endwhile ()
endfunction ()

# ctest is sensible to extension. ONLY WORKS with ".f90" AND NOT with ".F90"
add_fortran_test_executable (mtlnsolver_tests
    "test_init.f90"
)

get_target_property(mtlnsolver_INCLUDES mtlnsolver INCLUDE_DIRECTORIES)

# include_directories    (test PRIVATE 
#     ${mtlnsolver_INCLUDES}
# )

# target_link_directories(mtlnsolver_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../external/lapack)
target_link_libraries  (mtlnsolver_tests mtlnsolver)
# target_link_libraries  (mtlnsolver_tests lapack blas)
# target_link_libraries  (mtlnsolver_tests ${CMAKE_CURRENT_SOURCE_DIR}/../external/lapack/liblapack.lib ${CMAKE_CURRENT_SOURCE_DIR}/../external/lapack/libblas.lib)

