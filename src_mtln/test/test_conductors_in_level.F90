function test_conductors_in_level() result(error_cnt)

    use mtl_mod 
    use preprocess_mod, only: conductorsInLevel, findConductorsBeforeCable
    real, dimension(1,1) :: l1,c1,r1,g1
    real, dimension(2,2) :: l2,c2,r2,g2
    real, dimension(1,1) :: node1
    real, dimension(2,2) :: node2
    integer, dimension(1) :: divisions

    type(mtl_t) :: line1, line2, line3_1, line3_2, line4
    type(line_bundle_t) :: line_bundle
    error_cnt = 0
  
    line1   = mtl_t(l1, c1, r1, g1, node1, divisions, "line1")
    line2   = mtl_t(l2, c2, r2, g2, node2, divisions, "line2", "line1", 1)
    line3_1 = mtl_t(l2, c2, r2, g2, node2, divisions, "line3_1", "line2", 1)
    line3_2 = mtl_t(l2, c2, r2, g2, node2, divisions, "line3_2", "line2", 2)
    line4   = mtl_t(l2, c2, r2, g2, node2, divisions, "line4", "line3_2", 2)

    allocate(line_bundle%levels(4))
    line_bundle%levels(1)%lines = [line1]
    line_bundle%levels(2)%lines = [line2]
    line_bundle%levels(3)%lines = [line3_1, line3_2]
    line_bundle%levels(4)%lines = [line4]

    
    if (all(conductorsInLevel(line_bundle) /= [1,2,4,2])) then 
        error_cnt = error_cnt + 1
    end if

    write(*,*) error_cnt

end function