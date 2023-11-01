subroutine buildExpectedGrid(res)
    use NFDETypes

    type(Desplazamiento), intent(out) :: res

    res%nX = 10
    res%nY = 10
    res%nZ = 10

    return
end subroutine

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
        
    problem = readProblemDescription(filename)
    
    if (.not. associated(problem%despl)) stop 'Despl not initialized.'  
    call buildExpectedGrid(expectedGrid)
    if (.not. expectedGrid == problem%despl) stop 'Expected and read grids do not match'
        
end function