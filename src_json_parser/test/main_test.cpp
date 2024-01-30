#include <gtest/gtest.h>

extern "C" int test_fhash();

extern "C" int test_cells();

extern "C" int test_mesh_add_get();
extern "C" int test_mesh_node_to_pixel();
extern "C" int test_mesh_polyline_to_linel();


int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

TEST(fhash, basic_operation) {EXPECT_EQ(0, test_fhash()); }
// TEST(fhahs, idchildtable) {EXPECT_EQ(0, test_idchil)}

TEST(mesh, cells)             { EXPECT_EQ(0, test_cells()); }
TEST(mesh, add_get)           { EXPECT_EQ(0, test_mesh_add_get()); }
TEST(mesh, node_to_pixel)     { EXPECT_EQ(0, test_mesh_node_to_pixel()); }
TEST(mesh, polyline_to_linel) { EXPECT_EQ(0, test_mesh_polyline_to_linel()); }