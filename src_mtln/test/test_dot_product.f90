integer function test_dot_product() result(error_cnt)    
    use testingTools_mod

    implicit none
    ! integer, parameter :: DP = real64
    ! double precision, dimension(:,:), allocatable :: mat
    ! real(DP), dimension(:), allocatable :: ev_real, ev_imag
    ! real(DP), dimension(:), allocatable :: ev
    ! integer :: i
    real, dimension(:), allocatable :: x,y
    real :: dot
    error_cnt = 0
    x = [-0.0]
    y = [-0.0]
    x = [x, 1.0,1.0,2.0]
    y = [y, 0.5,3.0,4.0]

    dot = dot_product(x,y)
    write(*,*) dot
end function