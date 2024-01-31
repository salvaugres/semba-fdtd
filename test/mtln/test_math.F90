integer function test_math_eigvals() bind(C) result(error_cnt)    
    use utils_mod
    use testingTools_mod

    use iso_fortran_env, only: real64
    implicit none
    
    integer, parameter :: DP = real64
    double precision, dimension(:,:), allocatable :: mat
    real(DP), dimension(:), allocatable :: ev_real, ev_imag
    real(DP), dimension(:), allocatable :: ev
    integer :: i
    error_cnt = 0

    mat = reshape(source = [[0.35, 0.45,-0.14,-0.17] ,&
                            [0.09,0.07,-0.54,0.35]   ,&
                            [-0.44,-0.33,0.03,0.17]  ,&
                            [0.25,-0.32,-0.13,0.11]] , shape = [4,4], order = [2,1] )
    allocate(ev(8), ev_real(4), ev_imag(4))
    ev = getEigenValues(mat)
    ev_real = ev(1:4)
    ev_imag = ev(5:8)
    if (.not.(checkNear_dp(0.81630361_dp, ev_real(1), 0.005)) .or. .not.(checkNear_dp(0.0_dp, ev_imag(1), 0.005)) .or. &
        .not.(checkNear_dp(-0.0988341_dp, ev_real(2), 0.005)) .or. .not.(checkNear_dp(0.41323483_dp, ev_imag(2), 0.005)) .or. &
        .not.(checkNear_dp(-0.0988341_dp, ev_real(3), 0.005)) .or. .not.(checkNear_dp(-0.41323483_dp, ev_imag(3), 0.005)) .or. &
        .not.(checkNear_dp(-0.05863542_dp, ev_real(4), 0.005)) .or. .not.(checkNear_dp(0.0_dp, ev_imag(4), 0.005))) then
        error_cnt = 1
    endif  

end function

integer function test_math_matmul_broadcast() bind(C) result(error_cnt)    
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