integer function test_mesh() result(error_cnt)
   use mesh_mod
   use testingTools

   implicit none

   type(mesh_t) :: mesh


   integer :: stat
   class(*), allocatable :: data
   error_cnt = 0


   call mesh%coordinates%set(key(10), value=coordinate_t([0.0, 0.0, 0.0]))
   call mesh%coordinates%set(key(11), value=coordinate_t([1.0, 0.0, 0.0]))
   call mesh%coordinates%set(key(12), value=coordinate_t([2.0, 0.0, 0.0]))
   call mesh%coordinates%set(key(13), value=coordinate_t([3.0, 0.0, 0.0]))

   block
      type(node_t) :: expected = node_t(10), obtained
      
      call mesh%elements%set(key(1), value=expected)

      call mesh%elements%get_raw(key(1), data, stat)
      select type(data)
       type is (node_t)
         obtained = data
       class default
         error_cnt = error_cnt + 1
      end select
      if (obtained%coordIds /= expected%coordIds) error_cnt = error_cnt + 1
   end block

   block
      type(polyline_t) :: expected, obtained
      expected%coordIds = [11, 12, 13]

      call mesh%elements%set(key(2), value=expected)
      
      call mesh%elements%get_raw(key(2), data, stat)
      select type(data)
       type is (polyline_t)
         obtained = data
       class default
         error_cnt = error_cnt + 1
      end select
      if (any(obtained%coordIds /= expected%coordIds)) error_cnt = error_cnt + 1
   end block

end function

