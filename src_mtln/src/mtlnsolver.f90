module mtlnsolver

    use NFDETypes
    
    implicit none

    type, public :: mtl
        character (len=:), allocatable :: name
        ! character(LEN=MAX_LINEA) :: name
        ! integer  :: numberOfConductors
        ! real, allocatable :: lpul(:,:,:), cpul(:,:,:), rpul(:,:,:), gpul(:,:,:)
        ! real, allocatable :: u(:), du(:), duNorm(:)
        real :: time, dt
        ! real, allocatable :: longitudinalE(:,:), transversalE(:,:)
        ! real, allocatable :: vTerm(:,:,:), iTerm(:,:,:)
        
    contains
        procedure :: setTimeStep        

    end type
    
    interface mtl
        module procedure mtl_ctor
    end interface
            

contains

    function mtl_ctor(name) result(res)
        type(mtl) :: res
        character(len=*), intent(in) :: name

        res%name = name

    end function mtl_ctor
      

    function getPhaseVelocities(this) result(res)
        class(mtl) :: this
        real, allocatable :: res(:,:)
        !TODO
    end function getPhaseVelocities

    function getMaxTimeStep(this) result(res)
        class(mtl) :: this
        real :: res
        !TODO
    end function getMaxTimeStep

    ! subroutine initLC        
    ! end subroutine initLC

    ! subroutine initRG        
    ! end subroutine initRG

    subroutine setTimeStep(this, numberOfSteps, finalTime)
        class(mtl) :: this
        integer, intent(in) :: numberOfSteps
        real, intent (in) ::finalTime
        
        this% dt = finalTime/numberOfSteps

    end subroutine setTimeStep        


end module