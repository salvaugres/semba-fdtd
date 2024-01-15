module network_mod

    ! use fhash, only: fhash_tbl_t
    use mtl_bundle_mod

    implicit none
    type, public :: network_t
        integer :: number_of_nodes, number_of_state_vars
        real, dimension(:), allocatable :: v, i
        type(fhash_tbl_t) :: connections
        real, dimension(:), allocatable :: X, H
        real, dimension(:,:), allocatable :: P1, Ps, M, N1, Ns, O, Q1, Qs

    contains

    procedure, private :: getNumberOfStateVarsInConnnector
    procedure, private :: getNumberOfStateVars
    procedure, private :: addNodesInLine
    procedure, private :: addNode
    procedure, private :: iFactor
    ! procedure :: connectToGround
    ! procedure :: connectToGrounR
    ! procedure :: connectToGroundLCpRs
    ! procedure :: connectToGroundC
    ! procedure :: connectToGroundC
    ! procedure :: shortToGround
    ! procedure :: shortNodes
    procedure :: updateSources => network_updateSources
    ! procedure :: connectNodes
    procedure :: advanceVoltage => network_advanceVoltage
    procedure :: updateVoltages => network_updateVoltages
    procedure :: updateCurrents => network_updateCurrents
    procedure :: computeNWVoltageTerms => network_computeNWVoltageTerms


    end type network_t

    interface network_t
        module procedure networkCtor
    end interface


contains

    function networkCtor(network_description) result(res)
        type(fhash_tbl_t), intent(in) :: network_description
        type(network_t) :: res
    end function

    function getNumberOfStateVars(this, connections) result(res)
        class(network_t) :: this
        type(fhash_tbl_t) :: connections
        integer :: res
    end function

    function getNumberOfStateVarsInConnnector(this, connections) result(res)
        class(network_t) :: this
        type(fhash_tbl_t) :: connections
        integer :: res
    end function

    subroutine addNodesInLine(this)
        class(network_t) :: this
    end subroutine

    subroutine addNode(this)
        class(network_t) :: this
    end subroutine

    function iFactor(this,node) result(res)
        class(network_t) :: this
        type(fhash_tbl_t) :: node
        integer :: res
    end function


    subroutine computeVoltageTerms(this)
        class(network_t) :: this
        !TODO
    end subroutine

    subroutine network_updateSources(this, time, dt)
        class(network_t) :: this
        real, intent(in) :: time, dt
        !TODO
    end subroutine

    subroutine network_advanceVoltage(this, dt)
        class(network_t) :: this
        real, intent(in) :: dt
        !TODO
    end subroutine

    subroutine network_updateVoltages(this, bundles)
        class(network_t) :: this
        class(fhash_tbl_t), intent(in) :: bundles
        !TODO
    end subroutine

    subroutine network_updateCurrents(this, bundles)
        class(network_t) :: this
        class(fhash_tbl_t), intent(in) :: bundles
        !TODO
    end subroutine

    subroutine network_computeNWVoltageTerms(this, dt)
        class(network_t) :: this
        real, intent(in) :: dt
        !TODO
    end subroutine 





end module network_mod