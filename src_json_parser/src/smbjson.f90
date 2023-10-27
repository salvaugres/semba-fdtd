module smbjson
    
    use nfdetypes
    use json_module, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit
    
    implicit none
     
    private
    
    public :: readProblemDescription
    
contains
    function readGrid(p) result (res)
        type(Desplazamiento), intent(out) :: res
        
        type(json_value),pointer,intent(in) :: p
        type(json_value), pointer :: q
        
        
        call json%get(p,'grid',q)
        
        return
    end function
    
    function readProblemDescription(filename) result (problemDescription)
        character (len=*), intent(in) :: filename
        type(Parseador) :: pD !! Problem Description
        
        type(json_file) :: jsonFile       !! the JSON structure read from the file
        type(json_core) :: json
        
        call jsonFile%initialize()
        if (jsonFile%failed()) then
            call jsonFile%print_error_message(error_unit)
        end if
        
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
        
        pD%despl = readGrid(json)
        
        return
    end function
end module