integer function test_init() result(error_cnt)    
    use mtlnsolver

    implicit none

    character(len=*), parameter :: name = 'line0'

    type(mtl) :: line
    
    line = mtl()
    ! line%setTimeStep(10, 1e-3)


end function