integer function test_read_planewave() result(error_cnt)
    
    use smbjson
    use NFDETypes
    use NFDETypes_extension
    
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
    
    if (.not. associated(problem%despl)) stop 'Despl not initialized.'  
    if (.not. expectedGrid == problem%despl) stop 'Expected and read grids do not match'
        
end function
    
