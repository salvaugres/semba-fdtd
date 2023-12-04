module mtlnsolver

    use NFDETypes
    
    implicit none

    type, public :: mtl
        real, allocatable :: lpul(:,:,:), cpul(:,:,:), rpul(:,:,:), gpul(:,:,:)
    end type

contains

    function sumone(n) result(sum)
        implicit none
        integer, intent(in) :: n
        integer ::  sum
        
        sum = n+1

    end function sumone

end module