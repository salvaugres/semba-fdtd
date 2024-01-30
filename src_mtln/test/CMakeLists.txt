cmake_minimum_required(VERSION 3.5)
enable_language (CXX Fortran)
enable_testing ()

add_library(
    mtln_solver_tests_fortran
    "testingTools.f90"
    "test_init_homogeneous.F90"
    "test_init_inhomogeneous.F90"
    "test_eigvals.F90"
    "test_time_step.F90"
    "test_fhash.F90"
    "test_fhash_ptr.F90"
    "test_dot_product.F90"
    "test_elemental_component_sum.F90"
    "test_4dim_component_sum.F90"
    "test_matmul_broadcast.F90"
    "test_sum_derived_types.F90"
    "test_q3Phi.F90"
    "test_spice_dc.F90"
    "test_spice_ac.F90"
    "test_spice_current_source.F90"
    "test_spice_short.F90"
    "test_networks.F90"
    "test_conductors_in_level.F90"
    "test_conductors_before_cable.F90"
    "test_zt_conductor_ranges.F90"
    "test_zt_conductor_ranges_2.F90"
)
target_link_libraries(mtln_solver_tests_fortran
    mtlnsolver 
    fhash   
)

add_executable(mtlnsolver_tests "main_test.cpp")
target_link_libraries(mtlnsolver_tests
    mtln_solver_tests_fortran
    GTest::gtest_main
)