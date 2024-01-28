#include <gtest/gtest.h>

extern "C" int test_init_homogeneous();

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

TEST(Init_homogeneous, Init_homogeneous)
{
	EXPECT_EQ(test_init_homogeneous(), 0);
}