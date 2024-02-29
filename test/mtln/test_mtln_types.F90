integer function test_mtln_types() bind(C) result(err)

   use mtln_types_mod

   implicit none

   err = 0
   
   block
      type(termination_t) :: a, b

      if (.not. a == b) err = err + 1
   end block

end function

