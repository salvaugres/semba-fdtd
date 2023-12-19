integer function test_matmul_broadcast() result(error_cnt)    
    use testingTools_mod

    implicit none
    integer :: i

    real, dimension(:,:,:), allocatable :: A,B,res
    allocate(A(3,2,2))
    allocate(B(3,2,2))
    allocate(res(3,2,2))
    A(:,1,1) = 1.0
    A(:,2,1) = 0.0
    A(:,1,2) = 0.0
    A(:,2,2) = -1.0

    B(:,1,1) = 1.5
    B(:,2,1) = 0.0
    B(:,1,2) = 0.5
    B(:,2,2) = -1.0

    do i = 1,3
        res(i,:,:) = matmul(A(i,:,:),B(i,:,:))    
    enddo
        ! [(matmul(this%duNorm(i), this%lpul(i)), i = 1, this%number_of_divisions)]

    
    write(*,*) 'All res'
    write(*,*) res(1,:,:)
    write(*,*) res(2,:,:)
    write(*,*) res(3,:,:)
    
    res = reshape(source=[(matmul(A(i,:,:),B(i,:,:)), i = 1,3)], shape=[3,2,2], order=[2,3,1])

    write(*,*) 'after'
    write(*,*) res(1,:,:)
    write(*,*) res(2,:,:)
    write(*,*) res(3,:,:)

end function