#include <gtest/gtest.h>

extern "C" int test_map_wires_to_bundles();
extern "C" int test_json_to_solver_input_shielded_pair();

TEST(system, json_to_solver_shielded)     {EXPECT_EQ(0, test_json_to_solver_input_shielded_pair()); }

