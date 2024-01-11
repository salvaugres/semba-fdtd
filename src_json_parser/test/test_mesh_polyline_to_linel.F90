integer function test_mesh_polyline_to_linel() result(err)
   use mesh_mod
   use cells_mod
   use testingTools

   implicit none

   type(mesh_t) :: mesh

   err = 0

   call mesh%addCoordinate(1, coordinate_t([0.0, 0.0, 0.0]))
   call mesh%addCoordinate(2, coordinate_t([3.0, 0.0, 0.0]))
   call mesh%addCoordinate(3, coordinate_t([3.0, 1.0, 0.0]))
   call mesh%addCoordinate(4, coordinate_t([3.0, 0.0, 0.0]))

   block
      !   Polyline 1 (valid linel conversion)
      !             3
      !             ^
      !             | 4
      !    1  - - > 2
      !       1 2 3
      type(linel_t), dimension(:), allocatable :: ls
      type(polyline_t) :: pl

      pl = polyline_t([1, 2, 3])
      ls = mesh%convertPolylineToLinels(pl)
      if (.not. allocated(ls)) err = err + 1
      if (size(ls) /= 4) err = err + 1

      if (.not. linel_t(cell=[1, 1, 1], orientation=DIR_X, tag="1") == ls(1)) err = err + 1
      if (.not. linel_t(cell=[2, 1, 1], orientation=DIR_X         ) == ls(2)) err = err + 1
      if (.not. linel_t(cell=[3, 1, 1], orientation=DIR_X         ) == ls(3)) err = err + 1
      if (.not. linel_t(cell=[4, 1, 1], orientation=DIR_Y, tag="3") == ls(4)) err = err + 1

      if (mesh%arePolylineSegmentsStraight(pl) .neqv. .true.) err = err + 1
   end block

   block
      !   Polyline 2 (valid linel conversion, reverse orientation)
      !             3
      !             | 1
      !    1  < - - 2
      !       4 3 2
      type(linel_t), dimension(:), allocatable :: ls
      type(polyline_t) :: pl

      pl = polyline_t([3, 2, 1])
      ls = mesh%convertPolylineToLinels(pl)

      if (.not. allocated(ls)) err = err + 1
      if (size(ls) /= 4) err = err + 1

      if (.not. linel_t(cell=[4, 1, 1], orientation=DIR_Y, tag="3") == ls(1)) err = err + 1
      if (.not. linel_t(cell=[3, 1, 1], orientation=DIR_X, tag="2") == ls(2)) err = err + 1
      if (.not. linel_t(cell=[2, 1, 1], orientation=DIR_X         ) == ls(3)) err = err + 1
      if (.not. linel_t(cell=[1, 1, 1], orientation=DIR_X, tag="1") == ls(4)) err = err + 1

      if (mesh%arePolylineSegmentsStraight(pl) .neqv. .true.) err = err + 1
   end block

   block
      !   Polyline 3 (invalid linel conversion, does not follow grid)
      !        3
      !      /
      !    1
      type(linel_t), dimension(:), allocatable :: ls
      type(polyline_t) :: pl
      pl = polyline_t([1, 3])
      ls = mesh%convertPolylineToLinels(pl)
      if (size(ls) /= 0) err = err + 1

      if (mesh%arePolylineSegmentsStraight(pl) .neqv. .false.) err = err + 1
   end block

   block
      !   Polyline 4 (polyline with coord in same place.)
      !    1  - - > 2,4
      !       1 2 4
      type(linel_t), dimension(:), allocatable :: ls
      type(polyline_t) :: pl
      pl = polyline_t([1, 2, 4])
      ls = mesh%convertPolylineToLinels(pl)
      if (.not. allocated(ls)) err = err + 1
      if (size(ls) /= 3) err = err + 1

      if (.not. linel_t(cell=[1, 1, 1], orientation=DIR_X, tag="1") == ls(1)) err = err + 1
      if (.not. linel_t(cell=[2, 1, 1], orientation=DIR_X         ) == ls(2)) err = err + 1
      if (.not. linel_t(cell=[3, 1, 1], orientation=DIR_Y, tag="4") == ls(3)) err = err + 1

      if (mesh%arePolylineSegmentsStraight(pl) .neqv. .true.) err = err + 1
   end block


end function

