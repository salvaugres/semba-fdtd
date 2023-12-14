module networks_mod

    ! use fhash, only: fhash_tbl_t
    use mtl_bundle_mod

    implicit none
    type, public :: network_t

    contains
    ! procedure :: connectToGround
    ! procedure :: connectToGrounR
    ! procedure :: connectToGroundLCpRs
    ! procedure :: connectToGroundC
    ! procedure :: connectToGroundC
    ! procedure :: shortToGround
    ! procedure :: shortNodes
    procedure :: updateSources
    ! procedure :: connectNodes
    procedure :: advanceVoltage
    procedure :: updateVoltages
    procedure :: updateCurrents
    procedure :: computeVoltageTerms
    ! private
    ! procedure :: getNumberOfStateVarsInConnnector
    ! procedure :: getNumberOfStateVars
    ! procedure :: addNodesInLine
    ! procedure :: addNode
    ! procedure :: iFactor

    end type network_t

    interface network_t
        module procedure networkCtor
    end interface


contains

    function networkCtor() result(res)
        type(network_t) :: res
    end function

    subroutine computeVoltageTerms(this)
        class(network_t) :: this
        !TODO
    end subroutine

    subroutine updateSources(this, time, dt)
        class(network_t) :: this
        real, intent(in) :: time, dt
        !TODO
    end subroutine

    subroutine advanceVoltage(this, dt)
        class(network_t) :: this
        real, intent(in) :: dt
        !TODO
    end subroutine

    subroutine updateVoltages(this, bundles)
        class(network_t) :: this
        class(mtl_bundle_t), dimension(:), intent(in) :: bundles
        !TODO
    end subroutine

    subroutine updateCurrents(this, bundles)
        class(network_t) :: this
        class(mtl_bundle_t), dimension(:), intent(in) :: bundles
        !TODO
    end subroutine

    




end module networks_mod