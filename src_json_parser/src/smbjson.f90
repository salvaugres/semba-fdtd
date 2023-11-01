module smbjson
    
    use nfdetypes
    use json_module
    use json_kinds
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit
    
    implicit none
     
    private
    
    public :: readProblemDescription
    
contains
    subroutine initializeProblemDescription(pD)
        type(Parseador), intent(inout) :: pD

        allocate(pD%general) 
        allocate(pD%matriz)  
        allocate(pD%despl)   
        allocate(pD%front) 
        allocate(pD%Mats)  
        allocate(pD%pecRegs) 
        allocate(pD%pmcRegs) 
        allocate(pD%DielRegs) 
        allocate(pD%LossyThinSurfs) 
        allocate(pD%frqDepMats) 
        allocate(pD%aniMats)
        allocate(pD%nodSrc) 
        allocate(pD%Sonda) 
        allocate(pD%oldSONDA)
        allocate(pD%BloquePrb)
        allocate(pD%VolPrb) 
        allocate(pD%tWires) 
        allocate(pD%sWires) 
        allocate(pD%tSlots) 
        allocate(pD%boxSrc) 
        allocate(pD%plnSrc) 

    end subroutine

    function readGrid(json) result (res)
        type(Desplazamiento) :: res
        type(json_file) :: json
        
        call json%get('grid.numberOfCells(1)',res%nX)
        call json%get('grid.numberOfCells(2)',res%nY)
        call json%get('grid.numberOfCells(3)',res%nZ)
        


        return
    end function
    
    function readProblemDescription(filename) result (res)
        character (len=*), intent(in) :: filename
        type(Parseador) :: res !! Problem Description
        
        type(json_file) :: json       !! the JSON structure read from the file
        
        call json%initialize()
        if (json%failed()) then
            call json%print_error_message(error_unit)
            stop
        end if
        
        call json%load(filename = filename)
        if (json%failed()) then
            call json%print_error_message(error_unit)
            stop
        end if
        
        ! call initializeProblemDescription(res)
        allocate(res%despl)
        res%despl = readGrid(json)

        return
    end function
end module