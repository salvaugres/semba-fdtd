module testingTools_mod
    use iso_c_binding
    implicit none
    
    character(len=*, kind=c_char), parameter :: PATH_TO_TEST_DATA = c_char_'testData/'

    contains
            
    function dotmatrixmul(a,b) result(res)
        real, dimension(:,:,:), intent(in) :: a
        real, dimension(:,:), intent(in) :: b
        real, dimension(:), allocatable :: res
        integer :: i,j
        
  
        allocate(res(size(a,1)))
        do i = 1, size(a,1)
           res(i) = 0.0
           do j = 1, size(a,2)
              res(i) = res(i) + dot_product(a(i,j,:),b(j,:))
           end do
        end do
     end function
  
  
    subroutine comparePULMatrices(error_cnt, m_line, m_input)
        integer, intent(inout) :: error_cnt
        real, intent(in), dimension(:,:,:) :: m_line
        real, intent(in), dimension(:,:) :: m_input
        integer :: i

        if (size(m_input, dim = 1) .ne. size(m_input, dim = 2)) then
            error_cnt = error_cnt + 1
            return
        end if   

        do i = 1, size(m_line, dim = 1)
            if (.not.ALL(m_line(i,:,:) == m_input(:,:))) then
                error_cnt = error_cnt + 1
            end if
        end do
        
    end subroutine 

    subroutine comparePULMatricesIH(error_cnt, m_line, m_input)
        integer, intent(inout) :: error_cnt
        real, intent(in), dimension(:,:,:) :: m_line
        real, intent(in), dimension(:,:,:) :: m_input
        integer :: i

        if (size(m_input, dim = 2) .ne. size(m_input, dim = 2)) then
            error_cnt = error_cnt + 1
            return
        end if   

        if (size(m_input, dim = 1) .ne. size(m_input, dim = 1)) then
            error_cnt = error_cnt + 1
            return
        end if   

        if (.not.ALL(m_line(:,:,:) == m_input(:,:,:))) then
            error_cnt = error_cnt + 1
        end if
        
    end subroutine 

    function checkNear_dp(target, number, rel_tol) result(is_near)
        double precision, intent(in) :: target, number
        real :: rel_tol
        logical :: is_near
        double precision :: abs_diff

        abs_diff = abs(target-number)
        if (abs_diff == 0.0) then
            is_near = .true.
        else 
            is_near = abs(target-number)/target < rel_tol
        endif

    end function 

    function checkNear(target, number, rel_tol) result(is_near)
        real, intent(in) :: target, number
        real :: rel_tol
        logical :: is_near
        real :: abs_diff

        abs_diff = abs(target-number)
        if (abs_diff == 0.0) then
            is_near = .true.
        else 
            is_near = abs(target-number)/target < rel_tol
        endif

    end function 

    function checkNear_real8(target, number, rel_tol) result(is_near)
        real(kind=8), intent(in) :: target, number
        real(kind=8) :: rel_tol
        logical :: is_near
        real(kind=8) :: abs_diff

        abs_diff = abs(target-number)
        if (abs_diff == 0.0) then
            is_near = .true.
        else 
            is_near = abs(target-number)/target < rel_tol
        endif

    end function 

 

 end module testingTools_mod
 