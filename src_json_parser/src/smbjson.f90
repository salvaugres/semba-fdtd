module smbjson
    
    use NFDETypes
    use NFDETypes_extension

    use json_module
    use json_kinds
    
    implicit none
    private
    public :: readProblemDescription

    ! LABELS
    ! type(NFDEGeneral) 
    character (len=*), parameter :: GENERAL         = "general"  
    character (len=*), parameter :: TIME_STEP       = "timeStep"  
    character (len=*), parameter :: NUMBER_OF_STEPS = "numberOfSteps"  
    ! type(Desplazamiento) 
    character (len=*), parameter :: GRID            = "grid"  
    character (len=*), parameter :: NUMBER_OF_CELLS = "numberOfCells" 
    character (len=*), parameter :: STEPS           = "steps" 
    
contains
    
    subroutine getRealVecAndStore(json, place, dest)
        type(json_file) :: json
        character (len=*), intent(in) :: place
        REAL (KIND=RK), DIMENSION (:), POINTER :: dest

        real(RK),dimension(:),allocatable :: vec
        logical :: found = .false.

        call json%get(place, vec, found)
        if (found) then
            allocate(dest(size(vec)))
            dest = vec
        endif
    end subroutine

    function readGrid(json) result (res)
        type(Desplazamiento) :: res
        type(json_file) :: json
        
        call json%get(GRID//'.'//NUMBER_OF_CELLS//'(1)',res%nX)
        call json%get(GRID//'.'//NUMBER_OF_CELLS//'(2)',res%nY)
        call json%get(GRID//'.'//NUMBER_OF_CELLS//'(3)',res%nZ)
        
        call getRealVecAndStore(json ,GRID//'.'//STEPS//'.x', res%desX)
        call getRealVecAndStore(json ,GRID//'.'//STEPS//'.y', res%desY)
        call getRealVecAndStore(json ,GRID//'.'//STEPS//'.z', res%desZ)

        return
    end function

    function readGeneral(json) result (res)
        type(NFDEGeneral) :: res
        type(json_file) :: json
        
        call json%get(GENERAL//'.'//TIME_STEP,       res%dt)
        call json%get(GENERAL//'.'//NUMBER_OF_STEPS, res%nmax)
    end function 

    function readProblemDescription(filename) result (res)
        use, intrinsic :: iso_fortran_env , only: error_unit

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
        
        call initializeProblemDescription(res)
        res%general = readGeneral(json)
        res%despl   = readGrid(json)
    end function
end module