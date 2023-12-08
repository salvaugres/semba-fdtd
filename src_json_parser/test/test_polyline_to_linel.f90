integer function test_polyline_to_linel() result(err_cnt)
   use mesh_mod
   use testingTools

   implicit none

   type(mesh_t) :: mesh

   err_cnt = 0

   call mesh%addCoordinate(1, coordinate_t([0.0, 0.0, 0.0]))
   call mesh%addCoordinate(2, coordinate_t([3.0, 0.0, 0.0]))
   call mesh%addCoordinate(3, coordinate_t([3.0, 1.0, 0.0]))

   block
      !   Polyline 1 (valid linel conversion)
      !             3
      !             ^
      !             | 4
      !    1  - - > 2
      !       1 2 3
      type(linel_t), dimension(:), allocatable :: ls

      ls = convertPolylineToLinels(mesh, polyline_t([1, 2, 3]))
      if (.not. allocated(ls)) err_cnt = err_cnt + 1
      if (size(ls) /= 4) err_cnt = err_cnt + 1

      if (.not. linel_t(cell=[1, 1, 1], orientation=DIR_X, tag="1") == ls(1)) err_cnt = err_cnt + 1
      if (.not. linel_t(cell=[2, 1, 1], orientation=DIR_X         ) == ls(2)) err_cnt = err_cnt + 1
      if (.not. linel_t(cell=[3, 1, 1], orientation=DIR_X         ) == ls(3)) err_cnt = err_cnt + 1
      if (.not. linel_t(cell=[4, 1, 1], orientation=DIR_Y, tag="3") == ls(4)) err_cnt = err_cnt + 1
   end block

   
   block
      !   Polyline 2 (valid linel conversion, reverse orientation)
      !             3
      !             | 1
      !    1  < - - 2
      !       4 3 2
      type(linel_t), dimension(:), allocatable :: ls

      ls = convertPolylineToLinels(mesh, polyline_t([3, 2, 1]))
      if (.not. allocated(ls)) err_cnt = err_cnt + 1
      if (size(ls) /= 4) err_cnt = err_cnt + 1

      if (.not. linel_t(cell=[4, 1, 1], orientation=DIR_Y, tag="3") == ls(1)) err_cnt = err_cnt + 1
      if (.not. linel_t(cell=[3, 1, 1], orientation=DIR_X, tag="2") == ls(2)) err_cnt = err_cnt + 1
      if (.not. linel_t(cell=[2, 1, 1], orientation=DIR_X         ) == ls(3)) err_cnt = err_cnt + 1
      if (.not. linel_t(cell=[1, 1, 1], orientation=DIR_X, tag="1") == ls(4)) err_cnt = err_cnt + 1
   end block

   block
      !   Polyline 3 (invalid linel conversion, does not follow grid)
      !        3
      !      /
      !    1
      class(linel_t), dimension(:), allocatable :: ls
      ls = convertPolylineToLinels(mesh, polyline_t([1, 3]))
      if (size(ls) /= 0) err_cnt = err_cnt + 1
   end block

end function

