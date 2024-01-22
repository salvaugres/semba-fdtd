module network_mod

    ! use fhash, only: fhash_tbl_t
    use mtl_bundle_mod
    use circuit_mod
    implicit none
    type, public :: network_t
        integer :: number_of_nodes
        real, dimension(:), allocatable :: v, i
        type(fhash_tbl_t) :: connections
        type(circuit_t) :: circuit
    contains

    procedure, private :: addNodesInLine
    procedure, private :: addNode
    procedure, private :: iFactor
    ! procedure :: updateSources => network_updateSources
    ! procedure :: connectNodes
    procedure :: advanceVoltage => network_advanceVoltage
    procedure :: updateBundlesVoltages => network_updateBundlesVoltages
    procedure :: updateCurrents => network_updateCurrents
    ! procedure :: computeNWVoltageTerms => network_computeNWVoltageTerms


    end type network_t

    interface network_t
        module procedure networkCtor
    end interface


contains

    function networkCtor(network_description) result(res)
        type(fhash_tbl_t), intent(in) :: network_description
        type(network_t) :: res
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
        ! TODO
    end function


    ! subroutine computeVoltageTerms(this)
    !     class(network_t) :: this
    !     ! TODO
    ! end subroutine

    ! subroutine network_updateSources(this, time, dt)
    !     class(network_t) :: this
    !     real, intent(in) :: time, dt
    !     ! TODO
    ! end subroutine

    subroutine network_advanceVoltage(this, dt)
        class(network_t) :: this
        real, intent(in) :: dt
        call this%circuit%step()
        this%circuit%time = this%circuit%time + this%circuit%dt
    end subroutine

    subroutine network_updateBundlesVoltages(this, bundles)
        class(network_t) :: this
        class(fhash_tbl_t), intent(in) :: bundles
        ! TODO: voltages from nw to bundle
        ! loop over external nodes
        ! get pointer to line attached to node
        ! line[.....].v[...] = this%circuit%getNodeVoltage("node")
    end subroutine

    subroutine network_updateCurrents(this, bundles)
        class(network_t) :: this
        class(fhash_tbl_t), intent(in) :: bundles
        ! TODO: currents from bundle to current
        ! loop over external nodes
        ! get pointer to line attached to node
        ! update current source using current in bundle
        ! call this%circuit%updateNodeCurrent(node, current)
    end subroutine

    ! subroutine network_computeNWVoltageTerms(this, dt)
    !     class(network_t) :: this
    !     real, intent(in) :: dt
    !     ! TODO
    ! end subroutine 





end module network_mod