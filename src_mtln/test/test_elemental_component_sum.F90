integer function test_elemental_component_sum() result(error_cnt)    
    use testingTools_mod

    implicit none
    integer :: i

    real, dimension(:,:), allocatable :: res
    
    type(entry), dimension(:,:,:), allocatable :: A
    real, dimension(:,:,:), allocatable :: Asum
    type(entry), dimension(:,:), allocatable :: B
    allocate(A(1,2,2))
    allocate(B(1,2))

    A(1,1,1)%x = [1.0, 3.0, 2.5]
    A(1,2,2)%x = [-1.0, 2.0]
    A(1,2,1)%x = [(0.0, i=1, size(A(1,1,1)%x))]
    A(1,1,2)%x = [(0.0, i=1, size(A(1,2,2)%x))]
    ! res = matmul(A,B)
    
    B(1,1)%x = [0.1, 0.2, 0.3]
    B(1,2)%x = [0.4, 0.5]

    res = dotmul(A,B)

    Asum = componentSum(A)
  

    write(*,*) Asum

end function