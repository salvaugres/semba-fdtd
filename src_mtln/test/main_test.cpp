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
extern "C" int test_q3Phi();
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

TEST(init, homogeneous)   { EXPECT_EQ(0, test_init_homogeneous()); }
TEST(init, inhomogeneous) { EXPECT_EQ(0, test_init_inhomogeneous()); }

