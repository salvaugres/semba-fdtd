module testingTools_mod
    use iso_c_binding
    use mtl_mod, only: mtl_t
    ! use mtl_bundle_mod
    implicit none
    
    character(len=*, kind=c_char), parameter :: PATH_TO_TEST_DATA = c_char_'testData/'

contains
    
    ! type(mtl_bundle_t) function buildBundleFromLines()

    type(mtl_t) function buildLineWithNConductors(n,name, parent_name, conductor_in_parent, dt) result(res)
    
        integer, intent(in) :: n
        character(len=*), intent(in) :: name
        real, intent(in), optional :: dt
        character(len=*), intent(in), optional :: parent_name
        integer, intent(in), optional :: conductor_in_parent
        ! type(mtl_t) :: res
        real, allocatable, dimension(:,:) :: lpul, cpul, rpul, gpul
        real, dimension(2,3) :: node_positions = reshape( &
        source = [ 0.0, 0.0, 0.0, 100.0, 0.0, 0.0], shape = [2,3], order=(/2,1/) )
        integer, dimension(1) :: ndiv = (/5/)
        integer :: i,j

        allocate(lpul(n,n), source = 0.0)
        allocate(cpul(n,n), source = 0.0)
        allocate(gpul(n,n), source = 0.0)
        allocate(rpul(n,n), source = 0.0)

        do i = 1, n
            do j = 1, n
                rpul(i,j) = 0.0
                gpul(i,j) = 0.0
                if (i==j) then
                    lpul(i,j) = 4.4712610E-07
                    cpul(i,j) = 2.242e-10
                else 
                    lpul(i,j) = 1.4863653E-07
                    cpul(i,j) = -7.453e-11
                end if
            end do
        end do
        if (present(dt) .and. .not.present(parent_name)) then
            res = mtl_t(lpul, cpul, rpul, gpul, node_positions, ndiv, name, & 
                        dt = dt)
        else if (.not.present(dt) .and. present(parent_name) ) then
            res = mtl_t(lpul, cpul, rpul, gpul, node_positions, ndiv, name, & 
                        parent_name= parent_name, &
                        conductor_in_parent=conductor_in_parent)
        else if (present(dt) .and. present(parent_name) ) then
            res = mtl_t(lpul, cpul, rpul, gpul, node_positions, ndiv, name, & 
                        parent_name= parent_name, &
                        conductor_in_parent=conductor_in_parent, &
                        dt = dt)
        else 
            res = mtl_t(lpul, cpul, rpul, gpul, node_positions, ndiv, name)
        end if
    end function    


    ! function dotmatrixmul(a,b) result(res)
    !     real, dimension(:,:,:), intent(in) :: a
    !     real, dimension(:,:), intent(in) :: b
    !     real, dimension(:), allocatable :: res
    !     integer :: i,j
        
  
    !     allocate(res(size(a,1)))
    !     do i = 1, size(a,1)
    !        res(i) = 0.0
    !        do j = 1, size(a,2)
    !           res(i) = res(i) + dot_product(a(i,j,:),b(j,:))
    !        end do
    !     end do
    !  end function
  
  
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
 