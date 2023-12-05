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


 end module testingTools_mod
 