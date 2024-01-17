integer function test_dot_product() result(error_cnt)    
    use testingTools_mod

    implicit none
    integer :: i

    real, dimension(:,:), allocatable :: res
    type(entry), dimension(:,:,:), allocatable :: A
    type(entry), dimension(:,:), allocatable :: B
    allocate(A(1,2,2))
    allocate(B(1,2))
    ! allocate((A(0,0)%x)(2))
    A(1,1,1)%x = [1.0, 3.0, 2.5]
    A(1,2,2)%x = [-1.0, 2.0]
    A(1,2,1)%x = [(0.0, i=1, size(A(1,1,1)%x))]
    A(1,1,2)%x = [(0.0, i=1, size(A(1,2,2)%x))]
    ! res = matmul(A,B)
    
    B(1,1)%x = [0.1, 0.2, 0.3]
    B(1,2)%x = [0.4, 0.5]

    res = dotmul(A,B)
    write(*,*) res

end function