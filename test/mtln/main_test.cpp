#include <gtest/gtest.h>

extern "C" int test_init_homogeneous();
extern "C" int test_init_inhomogeneous();
extern "C" int test_4dim_component_sum();
extern "C" int test_coaxial_line_paul_8_6_square();
extern "C" int test_conductors_before_cable();
extern "C" int test_conductors_in_level();
extern "C" int test_dot_product();
extern "C" int test_eigvals();
extern "C" int test_elemental_component_sum();
extern "C" int test_fhash_ptr();
extern "C" int test_fhash();
extern "C" int test_matmul_broadcast();
extern "C" int test_networks();
extern "C" int test_q3phi();
extern "C" int test_spice_ac();
extern "C" int test_spice_current_source();
extern "C" int test_spice_dc();
extern "C" int test_spice_short();
extern "C" int test_sum_derived_types();
extern "C" int test_time_step();
extern "C" int test_zt_conductor_ranges_2();
extern "C" int test_zt_conductor_ranges();

int main(int argc, char **argv) 
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

TEST(mtln, homogeneous)   { EXPECT_EQ(0, test_init_homogeneous()); }
TEST(mtln, inhomogeneous) { EXPECT_EQ(0, test_init_inhomogeneous()); }
TEST(mtln, component_sum_4dim) { EXPECT_EQ(0, test_4dim_component_sum()); }
TEST(mtln, coaxial_line_paul_8_6_square) { EXPECT_EQ(0, test_coaxial_line_paul_8_6_square()); }
TEST(mtln, conductors_before_cable) { EXPECT_EQ(0, test_conductors_before_cable()); }
TEST(mtln, conductors_in_level) { EXPECT_EQ(0, test_conductors_in_level()); }
TEST(mtln, dot_product) { EXPECT_EQ(0, test_dot_product()); }
TEST(mtln, eigvals) { EXPECT_EQ(0, test_eigvals()); }
TEST(mtln, elemental_component_sum) { EXPECT_EQ(0, test_elemental_component_sum()); }
TEST(mtln, fhash_ptr) { EXPECT_EQ(0, test_fhash_ptr()); }
TEST(mtln, fhash) { EXPECT_EQ(0, test_fhash()); }
TEST(mtln, matmul_broadcast) { EXPECT_EQ(0, test_matmul_broadcast()); }
TEST(mtln, networks) { EXPECT_EQ(0, test_networks()); }
TEST(mtln, q3Phi) { EXPECT_EQ(0, test_q3phi()); }
TEST(mtln, spice_ac) { EXPECT_EQ(0, test_spice_ac()); }
TEST(mtln, spice_current_source) { EXPECT_EQ(0, test_spice_current_source()); }
TEST(mtln, spice_dc) { EXPECT_EQ(0, test_spice_dc()); }
TEST(mtln, spice_short) { EXPECT_EQ(0, test_spice_short()); }
TEST(mtln, sum_derived_types) { EXPECT_EQ(0, test_sum_derived_types()); }
TEST(mtln, time_step) { EXPECT_EQ(0, test_time_step()); }
TEST(mtln, zt_conductor_ranges_2) { EXPECT_EQ(0, test_zt_conductor_ranges_2()); }
TEST(mtln, zt_conductor_ranges) { EXPECT_EQ(0, test_zt_conductor_ranges()); }