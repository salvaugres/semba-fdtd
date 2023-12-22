integer function test_mesh_add_get() result(error_cnt)
   use mesh_mod
   use testingTools

   implicit none

   type(mesh_t) :: mesh
   logical :: found

   error_cnt = 0

   call mesh%addCoordinate(10, coordinate_t([0.0, 0.0, 0.0]))
   call mesh%addCoordinate(11, coordinate_t([1.0, 0.0, 0.0]))
   call mesh%addCoordinate(12, coordinate_t([2.0, 0.0, 0.0]))
   call mesh%addCoordinate(13, coordinate_t([3.0, 0.0, 0.0]))

   block
      type(node_t) :: expected, obtained      
      expected%coordIds = [10]
      call mesh%addElement(1, expected)
      obtained = mesh%getNode(1, found)
      if (.not. found) error_cnt = error_cnt + 1
      if (any(obtained%coordIds /= expected%coordIds)) error_cnt = error_cnt + 1
   end block

   block 
      type(node_t) :: obtained
      obtained = mesh%getNode(102, found)
      if (found) error_cnt = error_cnt + 1
   end block

   block
      type(polyline_t) :: expected, obtained
      expected%coordIds = [11, 12, 13]
      call mesh%addElement(2, expected)
      obtained = mesh%getPolyline(2, found)
      if (.not. found) error_cnt = error_cnt + 1
      if (any(obtained%coordIds /= expected%coordIds)) error_cnt = error_cnt + 1
   end block

end function

