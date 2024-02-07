integer function test_networks_pointer_copy() bind(C) result(error_cnt)

    type node_t 
        real, pointer :: v
        real, pointer :: i
    end type

    type(node_t), dimension(2) :: nodes
    type(node_t), dimension(2) :: nodes_copy
    real, dimension(3), target :: v1, v2
    real, dimension(2), target :: i1, i2

    error_cnt = 0 

    nodes(1)%v => v1(3)
    nodes(1)%i => i1(2)
    
    nodes(2)%v => v2(1)
    nodes(2)%i => i2(1)

    v1 = [1,2,3]
    v2 = [4,5,6]

    i1 = [-1,-2]
    i2 = [-3,-4]

    ! write(*,*) 'v1: ', v1, ' i1: ', i1
    ! write(*,*) 'v2: ', v2, ' i2: ', i2

    nodes(1)%v = 2*nodes(1)%v
    nodes(2)%v = 2*nodes(2)%v
    i1(2) = 0.5*i1(2)
    i2(1) = 0.5*i2(1)

    ! write(*,*) 'nodes: '
    ! write(*,*) 'v: ', nodes(1)%v, ' ', nodes(2)%v
    ! write(*,*) 'i: ', nodes(1)%i, ' ', nodes(2)%i
    ! write(*,*) 'v1: ', v1, ' i1: ', i1
    ! write(*,*) 'v2: ', v2, ' i2: ', i2

    if ((v1(3) /= 6.0) .or. (v2(1) /= 8.0)) then 
        error_cnt = error_cnt + 1
    end if
    if ((nodes(1)%i /= -1.0) .or. (nodes(2)%i /=-1.5)) then 
        error_cnt = error_cnt + 1
    end if

    nodes_copy = nodes
    nodes_copy(1)%v = 2*nodes_copy(1)%v
    nodes_copy(2)%v = 2*nodes_copy(2)%v
    i1(2) = 0.5*i1(2)
    i2(1) = 0.5*i2(1)

    if ((v1(3) /= 12.0) .or. (v2(1) /= 16.0)) then 
        error_cnt = error_cnt + 1
    end if
    if ((nodes_copy(1)%i /= -0.5) .or. (nodes_copy(2)%i /=-0.75)) then 
        error_cnt = error_cnt + 1
    end if

end function