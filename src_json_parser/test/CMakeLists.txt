cmake_minimum_required (VERSION 3.18)

enable_language (CXX Fortran)
enable_testing ()

add_library (smbjson_test_fortran 
    "testingTools.F90" 
    
    "test_fhash.F90"
    "test_idchildtable.F90"
    
    "test_cells.F90"
    "test_mesh.F90"
    "test_parser.F90"
    
    "test_read_planewave.F90"
    "test_read_holland1981.F90"
    "test_read_currentinjection.F90"
)
target_link_libraries(smbjson_test_fortran
    smbjson 
    fhash 
    ${JSONFORTRAN_LIB}
)

add_executable(smbjson_tests 
    "main_test.cpp"
)

target_link_libraries(smbjson_tests
    smbjson 
    smbjson_test_fortran
    GTest::gtest_main
)

