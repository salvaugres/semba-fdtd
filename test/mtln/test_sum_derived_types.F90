integer function test_sum_derived_types() bind(C) result(err_cnt)
    use testingTools_mod

    implicit none
    integer :: i


    type(entry), dimension(:,:,:), allocatable :: A,B, res
    type(entry) :: n1,n2, m, m_target, res_target
    real, dimension(:,:,:), allocatable :: Asum

    err_cnt = 0
    allocate(A(1,2,2))
    allocate(B(1,2,2))

    A(1,1,1)%x = [1.0, 3.0, 2.5]
    A(1,2,2)%x = [-1.0, 2.0]
    A(1,2,1)%x = [(0.0, i=1, size(A(1,1,1)%x))]
    A(1,1,2)%x = [(0.0, i=1, size(A(1,2,2)%x))]
    B(1,1,1)%x = [1.0, 3.0, 2.5]
    B(1,2,2)%x = [-1.0, 2.0]
    B(1,2,1)%x = [(0.0, i=1, size(A(1,1,1)%x))]
    B(1,1,2)%x = [(0.0, i=1, size(A(1,2,2)%x))]

    n1%x = [1.0, 2.0]
    n2%x = [1.0, 2.0]

    m_target%x = [2.0, 4.0]
    res_target%x = [2.0, 6.0, 5.0]

    m = n1 + n2
    res = A+B

    if (.not.(m == m_target)) then
        err_cnt = err_cnt + 1
        write(*,*) 'not m'
    end if
    if (.not.(res(1,1,1) == res_target)) then
        err_cnt = err_cnt + 1
        write(*,*) 'not res'
    end if


end function