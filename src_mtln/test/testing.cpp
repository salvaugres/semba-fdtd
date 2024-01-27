#include <gtest/gtest.h>

extern "C" int test_init_homogeneous_();

TEST(Squared, squares)
{
	EXPECT_EQ(test_init_homogeneous_(), 0);
}