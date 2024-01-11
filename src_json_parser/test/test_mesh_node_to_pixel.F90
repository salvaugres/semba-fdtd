integer function test_mesh_node_to_pixel() result(err)
   use mesh_mod
   use cells_mod
   use testingTools

   implicit none

   type(mesh_t) :: mesh

   err = 0

   call mesh%addCoordinate(1, coordinate_t([0.0, 0.0, 0.0]))

   block
      ! Valid pixel conversion.
      type(pixel_t), dimension(:), allocatable :: pix
      pix = mesh%convertNodeToPixels(node_t([1]))
      if (.not. size(pix) == 1) err = err + 1
      if (.not. pixel_t(cell=[1, 1, 1], tag="1") == pix(1)) err = err + 1
   end block

   block
      ! Invalid pixel conversion (Coordinate not present).
      type(pixel_t), dimension(:), allocatable :: pix
      pix = mesh%convertNodeToPixels(node_t([4]))
      if (size(pix) /= 0) err = err + 1
   end block

end function

