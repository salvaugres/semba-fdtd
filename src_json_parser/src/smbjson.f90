module smbjson
    
    use nfdetypes
    use json_module, wp => json_RK, IK => json_IK, LK => json_LK
    use, intrinsic :: iso_fortran_env , only: error_unit, output_unit
    
    implicit none
     
    private
    
    public :: readProblemDescription
    
contains
    function readGrid(p) result (res)
        type(Desplazamiento) :: res
        
        type(json_value),pointer,intent(in) :: p
        type(json_value), pointer :: q
        
        
        !call json%get(p,'grid',q)
        
        return
    end function
    
    function readProblemDescription(filename) result (res)
        character (len=*), intent(in) :: filename
        type(Parseador) :: res !! Problem Description
        
        type(json_file) :: jsonFile       !! the JSON structure read from the file
        type(json_core) :: json
        
        call jsonFile%initialize()
        if (jsonFile%failed()) then
            call jsonFile%print_error_message(error_unit)
        end if
        
        allocate(res%general) 
        allocate(res%matriz)  
        allocate(res%despl)   
        allocate(res%front) 
        allocate(res%Mats)  
        allocate(res%pecRegs) 
        allocate(res%pmcRegs) 
        allocate(res%DielRegs) 
        allocate(res%LossyThinSurfs) 
        allocate(res%frqDepMats) 
        allocate(res%aniMats)
        allocate(res%nodSrc) 
        allocate(res%Sonda) 
        allocate(res%oldSONDA)
        allocate(res%BloquePrb)
        allocate(res%VolPrb) 
        allocate(res%tWires) 
        allocate(res%sWires) 
        allocate(res%tSlots) 
        allocate(res%boxSrc) 
        allocate(res%plnSrc) 
        
        !pD%despl = readGrid(json)
        
        return
    end function
end module