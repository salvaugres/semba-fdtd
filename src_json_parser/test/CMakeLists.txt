cmake_minimum_required (VERSION 3.18)

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
        string (REGEX REPLACE ".F90$" "" TESTNAME ${TFILE})
        mangle_fortran_name (C_TESTNAME ${TESTNAME})
        list (APPEND TMP ${C_TESTNAME})
    endforeach ()
    set (${MANGLED} ${TMP} PARENT_SCOPE)
endfunction()

function (add_fortran_test_executable TARGET)
    set (TEST_FILES ${ARGN})
    mangle_fortran_filename_list (TEST_FILES_MANGLED ${TEST_FILES})

    create_test_sourcelist (_ main.c ${TEST_FILES_MANGLED})

    add_library (${TARGET}_fortran ${TEST_FILES} "testingTools.F90")
    target_link_libraries (${TARGET}_fortran smbjson fhash jsonfortran)
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

add_fortran_test_executable (smbjson_tests
    "test_fhash.F90"
    "test_idchildtable.F90"
    
    "test_cells.F90"
    "test_mesh_add_get.F90"
    "test_mesh_polyline_to_linel.F90"
    "test_mesh_node_to_pixel.F90"

    "test_parser_tools_interval_to_coords.F90"
    "test_parser_ctor.F90"
    
    "test_read_planewave.F90"
    "test_read_holland1981.F90"
    "test_read_currentinjection.F90"
)

target_link_libraries  (smbjson_tests smbjson)

