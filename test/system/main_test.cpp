#include <gtest/gtest.h>

extern "C" int test_map_wires_to_bundles();

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

TEST(system, map_wires_to_bundles)     {EXPECT_EQ(0, test_map_wires_to_bundles()); }

