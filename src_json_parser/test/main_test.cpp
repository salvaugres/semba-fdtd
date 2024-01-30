#include <gtest/gtest.h>

//extern "C" int test_cells();
//extern "C" int test_fhash();

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

TEST(test, test)
{
    EXPECT_TRUE(true);
}

//TEST(Cells, cells)
//{
//	EXPECT_EQ(test_cells(), 0);
//}
//
//TEST(fhash, fhash)
//{
//	EXPECT_EQ(test_cells(), 0);
//}