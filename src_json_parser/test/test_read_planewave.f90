integer function test_read_planewave() result(error_cnt)
    
    use smbjson
    use NFDETypes
    
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit
    
    implicit none

    character(len=*),parameter :: filename = 'planewave.fdtd.json'
    type(Parseador) :: problem
    type(Desplazamiento) :: expectedGrid
    logical :: areSame
    error_cnt = 0
    
    expectedGrid%nX = 10
    expectedGrid%nY = 10
    expectedGrid%nZ = 10
    
    problem = readProblemDescription(filename)
    
    if (.not. associated(problem%despl)) then
        error_cnt = 1
        return
    end if
    
    areSame = .true.
    areSame = areSame .and. (expectedGrid%nX == problem%despl%nX)
    areSame = areSame .and. (expectedGrid%nY == problem%despl%nY)
    areSame = areSame .and. (expectedGrid%nZ == problem%despl%nZ)
    
    if (.not. areSame) then
        error_cnt = 1
        return
    end if
        
end function
    
