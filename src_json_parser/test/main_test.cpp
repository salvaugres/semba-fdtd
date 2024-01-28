#include <gtest/gtest.h>

extern "C" int test_cells();

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

TEST(Cells, cells)
{
	EXPECT_EQ(test_cells(), 0);
}