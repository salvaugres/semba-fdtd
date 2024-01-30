integer function test_networks() result(error_cnt)

    type node_t 
        real, pointer :: v
        real, pointer :: i
    end type

    type(node_t), dimension(2) :: nodes
    real, dimension(3), target :: v1, v2
    real, dimension(2), target :: i1, i2

    

    nodes(1)%v => v1(3)
    nodes(1)%i => i1(2)
    
    nodes(2)%v => v2(1)
    nodes(2)%i => i2(1)

    v1 = [1,2,3]
    v2 = [4,5,6]

    i1 = [-1,-2]
    i2 = [-3,-4]

    write(*,*) 'v1: ', v1, ' i1: ', i1
    write(*,*) 'v2: ', v2, ' i2: ', i2

    nodes(1)%v = 2*nodes(1)%v
    nodes(2)%v = 2*nodes(2)%v
    i1(2) = 0.5*i1(2)
    i2(1) = 0.5*i2(1)

    write(*,*) 'nodes: '
    write(*,*) 'v: ', nodes(1)%v, ' ', nodes(2)%v
    write(*,*) 'i: ', nodes(1)%i, ' ', nodes(2)%i
    write(*,*) 'v1: ', v1, ' i1: ', i1
    write(*,*) 'v2: ', v2, ' i2: ', i2
end function