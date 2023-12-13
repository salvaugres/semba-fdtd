module testingTools_mod
    implicit none


    contains

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
        
    end subroutine comparePULMatrices

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

    end function checkNear_dp

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

    end function checkNear

 end module testingTools_mod
 