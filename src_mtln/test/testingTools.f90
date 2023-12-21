module testingTools_mod


    implicit none
    type :: entry
        real, dimension(:), allocatable :: x
    end type entry

    
    interface operator(+)
        module procedure add_entries
    end interface
    
    interface operator(==)
        module procedure compare_entries
    end interface
        
    interface operator(*)
        module procedure dotmul
    end interface operator(*)
            
            
    contains
            
    elemental function add_entries(a,b) result(res)
        type(entry), intent(in) :: a,b
        type(entry) :: res
        res%x = a%x + b%x
    end function

    elemental function compare_entries(a,b) result(res)
        type(entry), intent(in) :: a,b
        logical :: res
        integer :: i
        res = .true.
        do i = 1, size(a%x)
            if (a%x(i) /= b%x(i)) then
                res = .false.
                return
            end if
        end do
    end function

    elemental function componentSum(a) result(res)
        type(entry), intent(in) :: a
        real :: res
        res = sum(a%x)
    end function


    function dotmul(a,b) result(res)
        type(entry), dimension(:,:,:), intent(in) :: a
        type(entry), dimension(:,:), intent(in) :: b
        type(entry), dimension(:,:,:),allocatable :: breshaped
  
        real, dimension(size(A,1), size(A,2), 1) :: res_temp
        real, dimension(size(A,1), size(A,2)) :: res
        integer :: i,j,k,nz
        real :: tmp
        
        breshaped = reshape(b, [size(b,1),size(b,2),1])
        do nz = 1, size(a,1)
           do j=1,size(breshaped,3)
              do i=1,size(A,2)
                    tmp = 0.0 
                    do k=1,size(A,3)
                       tmp = tmp + dot_product(a(nz,i,k)%x, breshaped(nz,k,j)%x)
                    enddo
                    res_temp(nz,i,j) = tmp
              enddo
           enddo
        enddo
        
        res = reshape(res_temp, [size(b,1),size(b,2)])
  
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

 end module testingTools_mod
 