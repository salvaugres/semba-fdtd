function test_conductors_before_cable() bind(C) result(error_cnt)

    use mtl_mod 
    use preprocess_mod, only: conductorsInLevel, findConductorsBeforeCable
    real,dimension(2,2) :: l2 = reshape( source = [ 4.4712610E-07, 1.4863653E-07, 1.4863653E-07, 4.4712610E-07 ], shape = [ 2,2 ] )
    real,dimension(2,2) :: c2 = reshape( source = [ 2.242e-10, -7.453e-11,-7.453e-11, 2.242e-10 ], shape = [ 2,2 ] )
    real,dimension(2,2) :: r2 = reshape( source = [ 0.0, 0.0, 0.0, 0.0 ], shape = [ 2,2 ] )
    real,dimension(2,2) :: g2 = reshape( source = [ 0.0, 0.0, 0.0, 0.0 ], shape = [ 2,2 ] )
    real,dimension(1,1) :: l1 = reshape( source = [ 4.4712610E-07 ], shape = [ 1,1 ] )
    real,dimension(1,1) :: c1 = reshape( source = [ 2.242e-10 ], shape = [ 1,1 ] )
    real,dimension(1,1) :: r1 = reshape( source = [ 0.0 ], shape = [ 1,1 ] )
    real,dimension(1,1) :: g1 = reshape( source = [ 0.0 ], shape = [ 1,1 ] )
    real, dimension(1,1) :: node1
    real, dimension(2,2) :: node2
    integer, dimension(1) :: divisions

    type(mtl_t) :: line1, line2, line3_1, line3_2, line4
    type(line_bundle_t) :: line_bundle
    error_cnt = 0
  
    line1   = mtl_t(l1, c1, r1, g1, node1, divisions, "line1")
    line2   = mtl_t(l2, c2, r2, g2, node2, divisions, "line2",   parent_name = "line1",   conductor_in_parent = 1)
    line3_1 = mtl_t(l2, c2, r2, g2, node2, divisions, "line3_1", parent_name = "line2",   conductor_in_parent = 1)
    line3_2 = mtl_t(l2, c2, r2, g2, node2, divisions, "line3_2", parent_name = "line2",   conductor_in_parent = 2)
    line4   = mtl_t(l2, c2, r2, g2, node2, divisions, "line4",   parent_name = "line3_2", conductor_in_parent = 2)

    allocate(line_bundle%levels(4))
    line_bundle%levels(1)%lines = [line1]
    line_bundle%levels(2)%lines = [line2]
    line_bundle%levels(3)%lines = [line3_1, line3_2]
    line_bundle%levels(4)%lines = [line4]

    if (findConductorsBeforeCable("line2", line_bundle%levels(2)) /= 0) then 
        error_cnt = error_cnt + 1
    end if
    if (findConductorsBeforeCable("line3_1", line_bundle%levels(3)) /= 0) then 
        error_cnt = error_cnt + 1
    end if
    if (findConductorsBeforeCable("line3_2", line_bundle%levels(3)) /= 2) then 
        error_cnt = error_cnt + 1
    end if
    if (findConductorsBeforeCable("line4", line_bundle%levels(4)) /= 0) then 
        error_cnt = error_cnt + 1
    end if

    write(*,*) error_cnt

end function