#include <gtest/gtest.h>

extern "C" int test_mtl_init_homogeneous();
extern "C" int test_mtl_init_inhomogeneous();
extern "C" int test_mtl_time_step();
extern "C" int test_mtl_wrong_dt();
extern "C" int test_mtl_bundle_init();
extern "C" int test_fhash_ptr();
extern "C" int test_fhash();
extern "C" int test_math_eigvals();
extern "C" int test_math_matmul_broadcast();
extern "C" int test_networks_pointer_copy();
extern "C" int test_networks_simple_manager();
extern "C" int test_dispersive_init_1_pole();
extern "C" int test_dispersive_init_2_poles();
extern "C" int test_dispersive_init_1_pole_3_levels();
extern "C" int test_dispersive_init_1_pole_lines_with_lumped();
extern "C" int test_spice_tran();
// extern "C" int test_spice_tran_2();
extern "C" int test_spice_multiple();
extern "C" int test_spice_current_source();
extern "C" int test_spice_dc();
extern "C" int test_spice_read_message();
extern "C" int test_preprocess_conductors_before_cable();
extern "C" int test_preprocess_conductors_in_level();
extern "C" int test_preprocess_zt_conductor_ranges_2();
extern "C" int test_preprocess_zt_conductor_ranges();
// extern "C" int test_coaxial_line_paul_8_6_square();

int main(int argc, char **argv) 
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

TEST(mtln, mtl_homogeneous)   { EXPECT_EQ(0, test_mtl_init_homogeneous()); }
TEST(mtln, mtl_inhomogeneous) { EXPECT_EQ(0, test_mtl_init_inhomogeneous()); }
TEST(mtln, mtl_time_step) { EXPECT_EQ(0, test_mtl_time_step()); }
TEST(mtln, mtl_wrong_dt) { EXPECT_EQ(0, test_mtl_wrong_dt()); }
TEST(mtln, mtl_bundle_init) { EXPECT_EQ(0, test_mtl_bundle_init()); }
TEST(mtln, fhash_ptr) { EXPECT_EQ(0, test_fhash_ptr()); }
TEST(mtln, fhash) { EXPECT_EQ(0, test_fhash()); }
TEST(mtln, preprocess_conductors_before_cable) { EXPECT_EQ(0, test_preprocess_conductors_before_cable()); }
TEST(mtln, preprocess_conductors_in_level) { EXPECT_EQ(0, test_preprocess_conductors_in_level()); }
TEST(mtln, preprocess_zt_conductor_ranges_2) { EXPECT_EQ(0, test_preprocess_zt_conductor_ranges_2()); }
TEST(mtln, preprocess_zt_conductor_ranges) { EXPECT_EQ(0, test_preprocess_zt_conductor_ranges()); }
TEST(mtln, math_eigvals) { EXPECT_EQ(0, test_math_eigvals()); }
TEST(mtln, math_matmul_broadcast) { EXPECT_EQ(0, test_math_matmul_broadcast()); }
TEST(mtln, networks_simple_manager) { EXPECT_EQ(0, test_networks_simple_manager()); }
TEST(mtln, networks_pointer_copy) { EXPECT_EQ(0, test_networks_pointer_copy()); }
TEST(mtln, dispersive_init_1_pole) { EXPECT_EQ(0, test_dispersive_init_1_pole()); }
TEST(mtln, dispersive_init_2_poles) { EXPECT_EQ(0, test_dispersive_init_2_poles()); }
TEST(mtln, dispersive_init_1_pole_3_levels) { EXPECT_EQ(0, test_dispersive_init_1_pole_3_levels()); }
TEST(mtln, dispersive_init_1_pole_lines_with_lumped) { EXPECT_EQ(0, test_dispersive_init_1_pole_lines_with_lumped()); }
TEST(mtln, spice_tran) { EXPECT_EQ(0, test_spice_tran()); }
// TEST(mtln, spice_tran_2) { EXPECT_EQ(0, test_spice_tran_2()); }
TEST(mtln, spice_multiple) { EXPECT_EQ(0, test_spice_multiple()); }
TEST(mtln, spice_current_source) { EXPECT_EQ(0, test_spice_current_source()); }
TEST(mtln, spice_dc) { EXPECT_EQ(0, test_spice_dc()); }
TEST(mtln, spice_read_message) { EXPECT_EQ(0, test_spice_read_message()); }
// TEST(mtln, coaxial_line_paul_8_6_square) { EXPECT_EQ(0, test_coaxial_line_paul_8_6_square()); }