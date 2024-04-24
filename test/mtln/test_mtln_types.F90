integer function test_mtln_types() bind(C) result(err)

   use mtln_types_mod
   implicit none

   
   type(terminal_node_t) :: t
   class(termination_t), allocatable :: d
   
   err = 0
   allocate(termination_with_source_t :: t%termination)
   t%termination = termination_with_source_t(path_to_excitation="path", &
                                             termination_type = TERMINATION_SERIES, &
                                             resistance = 150, &
                                             inductance = 0.0, &
                                             capacitance = 1e22)


   select type(d => t%termination)
   type is (termination_t)
      err = err + 1
   type is (termination_with_source_t)
      if (d%path_to_excitation /= "path") then 
          err = err + 1
      end if
   end select

   err = read_path(t, err)

   contains
   function read_path(node, err) result(res)
      type(terminal_node_t), intent(in) :: node
      integer, intent(in) :: err
      integer :: res
      class(termination_t), allocatable :: termination
      select type(termination => node%termination)
      type is(termination_with_source_t)
         if (termination%path_to_excitation /= "path") then 
            res = err + 1
         end if
      class default
         res = err + 1
      end select

   end function

end function

