integer function test_derived_type_submodule() bind(C) result(err)

   use mtln_solver_mod
   
   implicit none
   
   type(terminal_node_t) :: node
   type(terminal_connection_t) :: connection
   type(terminal_network_t) :: network
   type(parsed_mtln_t) :: parsed 
   character(len=256), parameter :: square_excitation = trim('coaxial_line_paul_8_6_0.25_square.exc')
   class(termination_t), allocatable :: t
   err = 0 

   allocate(termination_with_source_t :: node%termination)
   ! t => node%termination
   node%conductor_in_cable = 1
   node%side = TERMINAL_NODE_SIDE_INI
   ! node%termination%path_to_excitation = "A"
   node%termination = termination_with_source_t(path_to_excitation=trim(square_excitation), & 
                                                termination_type = TERMINATION_SERIES, &
                                                resistance = 150, &
                                                inductance = 0.0, &
                                                capacitance = 1e22)

   allocate(connection%nodes(1))                                                   
   connection%nodes(1) = node
   allocate(network%connections(1))
   network%connections(1) = connection
   allocate(parsed%networks(1))
   parsed%networks(1) = network

   write(*,*) 'Before entering module:'
   select type (t => parsed%networks(1)%connections(1)%nodes(1)%termination)
   type is (termination_t)
       err = err + 1
   type is (termination_with_source_t)
       write(*,*) '  path_to_excitation:'
       write(*,*) '  ', t%path_to_excitation
       if (t%path_to_excitation /= trim(square_excitation)) then
           err = err + 1
       end if
   end select

   err = testPath(parsed, trim(square_excitation), err)

   ! err = read_path(p%t, err)

   ! select type(d => p%t%termination)
   ! type is (termination_t)
   !    err = err + 1
   ! type is (termination_with_source_t)
   !    if (d%path_to_excitation /= "path") then 
   !        err = err + 1
   !    end if
   ! end select

end function

integer function test_mtln_types() bind(C) result(err)

   use mtln_types_mod
   implicit none

   
   type(terminal_node_t) :: t
   class(termination_t), allocatable :: d
   
   err = 0
   ! allocate(termination_with_source_t :: t%termination)
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

