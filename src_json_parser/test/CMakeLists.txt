
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

    add_library (${TARGET}_fortran ${TEST_FILES})
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
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/src_json_parser/testData/
        )
        math (EXPR INDEX "${INDEX} + 1")
    endwhile ()
endfunction ()

# ctest is sensible to extension. ONLY WORKS with ".f90" AND NOT with ".F90"
add_fortran_test_executable (fortran_ctest
    "test_fails_1.f90"
    "test_succeeds_1.f90"
    "test_jsonfortran.f90"
)

target_sources(fortran_ctest_fortran PRIVATE "utils.F90")

include_directories(fortran_ctest PRIVATE ${jsonfortran_INCLUDE_DIRS})
target_link_directories(fortran_ctest PRIVATE ${jsonfortran_LIBRARY_DIRS})
target_link_libraries(fortran_ctest jsonfortran)

