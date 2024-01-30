integer function test_4dim_component_sum() bind(C) result(error_cnt)    
    use testingTools_mod

    implicit none

    
    real, dimension(:,:,:,:), allocatable :: A
    real, dimension(:,:,:), allocatable :: Asum

    real, dimension(:,:,:,:), allocatable :: B
    real, dimension(:,:,:), allocatable :: Bsum
    integer :: nr, ndiv, nc
    integer :: i,j,k
    nr = 3
    ndiv = 2
    nc = 2

    allocate(A(nr,ndiv,nc,nc))
    A(:,:,:,:) = 0.0
    A(:,1,1,1) = 1.0
    A(1,2,2,2) = 2.0
    A(2,2,2,2) = 2.0
    ! A(1,1,1)%x = [1.0, 3.0, 2.5]
    ! A(1,2,2)%x = [-1.0, 2.0]
    ! A(1,2,1)%x = [(0.0, i=1, size(A(1,1,1)%x))]
    ! A(1,1,2)%x = [(0.0, i=1, size(A(1,2,2)%x))]
 
    Asum = sumAlongAxis(A)
    write(*,*) Asum

    allocate(B(ndiv,nc,nc,nr))
    B(:,:,:,:) = 0.0
    B(:,1,1,1:2) = 1.0
    B(1,2,2,:) = 2.0
    B(2,2,2,:) = 1.5
    Bsum = sumAlongLastAxis(B)
    do i = 1, ndiv
        write(*,*) Bsum(i,1,1), Bsum(i,1,2)
        write(*,*) Bsum(i,2,1), Bsum(i,2,2)
        write(*,*) '---'

    enddo   


end function