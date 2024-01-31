integer function test_q3phi() bind(C) result(error_cnt)    
    use testingTools_mod

    implicit none

    

    real, dimension(:,:,:,:), allocatable :: q3
    real, dimension(:,:,:), allocatable :: phi
    real, dimension(:,:), allocatable :: q3phi
    integer :: nr, ndiv, nc, i_div

    nr = 2
    ndiv = 3
    nc = 2

    allocate(q3(ndiv,nc,nc,nr))
    allocate(phi(ndiv,nc,nr))
    allocate(q3phi(ndiv,nc))

    q3(:,:,:,:) = 0.0
    q3(1,:,:,1:2) = 1.0
    q3(1,:,:,1:2) = 1.0
    
    q3(2,:,:,1:2) = 0.5
    q3(2,:,:,1:2) = 0.5
    
    q3(3,:,:,1:2) = 1.0
    q3(3,:,:,1:2) = 1.0

    phi(:,:,:) = 0.0
    phi(1,:,1:2) = 5.0
    phi(1,:,1:2) = 5.0

    phi(2,:,1:2) = 2.0
    phi(2,:,1:2) = 2.0

    phi(3,:,1:2) = 5.0
    phi(3,:,1:2) = 5.0

    q3phi(:,:) = reshape(source = [(dotmatrixmul(q3(i_div, :, :, :), phi(i_div, :, :)), i_div = 1, ndiv)], &
                                   shape=[ndiv, nc], order=[2,1])

    
    write(*,*) dotmatrixmul(q3(1, :, :, :), phi(1, :, :))
    write(*,*) dotmatrixmul(q3(2, :, :, :), phi(2, :, :))
    write(*,*)
    write(*,*) q3phi(1,1)
    write(*,*) q3phi(1,2)
    write(*,*) '---'
    write(*,*) q3phi(2,1)
    write(*,*) q3phi(2,2)
    write(*,*) '---'
    write(*,*) q3phi(3,1)
    write(*,*) q3phi(3,2)
    write(*,*)
    

end function