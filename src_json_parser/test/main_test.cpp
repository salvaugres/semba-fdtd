#include <gtest/gtest.h>

extern "C" int test_fhash();
extern "C" int test_idchildtable();

extern "C" int test_cells();
extern "C" int test_mesh_add_get();
extern "C" int test_mesh_node_to_pixel();
extern "C" int test_mesh_polyline_to_linel();

extern "C" int test_parser_ctor();

extern "C" int test_read_planewave();
extern "C" int test_read_holland1981();
extern "C" int test_read_currentinjection();

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

TEST(idchildtable, fhash)     {EXPECT_EQ(0, test_fhash()); }
TEST(idchildtable, add_get)   {EXPECT_EQ(0, test_idchildtable()); }

TEST(mesh, cells)             { EXPECT_EQ(0, test_cells()); }
TEST(mesh, add_get)           { EXPECT_EQ(0, test_mesh_add_get()); }
TEST(mesh, node_to_pixel)     { EXPECT_EQ(0, test_mesh_node_to_pixel()); }
TEST(mesh, polyline_to_linel) { EXPECT_EQ(0, test_mesh_polyline_to_linel()); }

TEST(parser, ctor)            { EXPECT_EQ(0, test_parser_ctor()); }

TEST(read, planewave)         { EXPECT_EQ(0, test_read_planewave()); }
TEST(read, holland1981)           { EXPECT_EQ(0, test_read_holland1981()); }
TEST(read, currentinjection)  { EXPECT_EQ(0, test_read_currentinjection()); }