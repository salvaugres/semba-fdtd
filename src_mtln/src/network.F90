module network_mod

    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    use mtl_bundle_mod
    use mtln_types_mod
    use circuit_mod
    implicit none

    type node_t
        real, pointer :: v
        real, pointer :: i
        character(len=:), allocatable :: name
    end type


    type, public :: network_t
        integer :: number_of_nodes = 0
        ! real, dimension(:), allocatable :: v, i
        ! type(fhash_tbl_t) :: connections
        type(node_t), dimension(:), allocatable :: nodes
        type(circuit_t) :: circuit
    contains

    procedure, private :: connectNodeToGround
    procedure, private :: connectNodes
    procedure, private :: updateNetworkVoltagesFromCircuit
    procedure, private :: updateCircuitCurrentsFromBundles

    ! procedure, private :: addNodesInLine
    ! procedure, private :: addNode
    ! procedure, private :: iFactor
    ! procedure :: updateSources => network_updateSources
    ! procedure :: connectNodes
    procedure :: advanceVoltage => network_advanceVoltage
    ! procedure :: updateBundlesVoltages => network_updateBundlesVoltages
    ! procedure :: updateCurrents => network_updateCurrents
    ! procedure :: computeNWVoltageTerms => network_computeNWVoltageTerms


    end type network_t

    interface network_t
        module procedure networkCtor
    end interface


contains

    integer function countNodes(connections)
        type(terminal_connection_t), dimension(:), allocatable :: connections
        integer :: i
        countNodes = 0
        do i = 1, size(connections)
            countNodes = countNodes + size(connections(i)%nodes)
        end do
    end function

    function networkCtor(terminal_network, cable_name_to_bundle) result(res)
        type(terminal_network_t), intent(in) :: terminal_network
        type(fhash_tbl_t) :: cable_name_to_bundle 
        type(network_t) :: res
        integer :: i

        res%number_of_nodes = countNodes(terminal_network%connections)
        allocate(res%nodes(0))
        ! allocate(res%nodes(res%number_of_nodes))
        do i = 1, size(terminal_network%connections)
            if (size(terminal_network%connections(i)%nodes) == 1) then 
                call res%connectNodeToGround(terminal_network%connections(i)%nodes, cable_name_to_bundle)
            else
                call res%connectNodes(terminal_network%connections(i)%nodes, cable_name_to_bundle)
            end if
        end do
    end function

    subroutine connectNodes(this, nodes, cable_name_to_bundle)
        class(network_t) :: this
        type(terminal_node_t), dimension(:), allocatable :: nodes
        type(fhash_tbl_t) :: cable_name_to_bundle 
        type(node_t) :: new_node
        integer :: i, stat
        class(*), pointer :: d

        integer :: conductor_number

        do i = 1, 2
            call cable_name_to_bundle%get_raw_ptr(key(nodes(i)%belongs_to_cable%name), d, stat)
            if (stat /= 0) return
            new_node%name = nodes(i)%belongs_to_cable%name//"_"//nodes(i)%side
            select type(d)
            type is (mtl_bundle_t)
                if (nodes(i)%side == "initial") then 
                    new_node%v => d%v(conductor_number, lbound(d%v,2))
                    new_node%i => d%i(conductor_number, lbound(d%i,2))
                else if (nodes(i)%side == "end") then 
                    new_node%v => d%v(conductor_number, ubound(d%v,2))
                    new_node%i => d%i(conductor_number, ubound(d%i,2))
                end if
            end select
            this%nodes = [this%nodes, new_node]
        end do
        ! lineas para circuit
    end subroutine

    subroutine connectNodeToGround(this, nodes, cable_name_to_bundle)
        class(network_t) :: this
        type(terminal_node_t), dimension(:), allocatable :: nodes
        type(fhash_tbl_t) :: cable_name_to_bundle 
        type(node_t) :: new_node
        integer :: stat
        class(*), pointer :: d

        integer :: conductor_number
        
        call cable_name_to_bundle%get_raw_ptr(key(nodes(1)%belongs_to_cable%name), d, stat)
        if (stat /= 0) return
        new_node%name = nodes(1)%belongs_to_cable%name//"_"//nodes(1)%side
        select type(d)
        type is (mtl_bundle_t)
            if (nodes(1)%side == "initial") then 
                new_node%v => d%v(conductor_number, lbound(d%v,2))
                new_node%i => d%i(conductor_number, lbound(d%i,2))
            else if (nodes(1)%side == "end") then 
                new_node%v => d%v(conductor_number, ubound(d%v,2))
                new_node%i => d%i(conductor_number, ubound(d%i,2))
            end if
        end select
        this%nodes = [this%nodes, new_node]
        ! falta las lineas para el circuit
        
    end subroutine

    subroutine updateNetworkVoltagesFromCircuit(this)
        class(network_t) :: this
    end subroutine
    subroutine updateCircuitCurrentsFromBundles(this)
        class(network_t) :: this
    end subroutine


    ! subroutine addNodesInLine(this)
    !     class(network_t) :: this
    ! end subroutine

    ! subroutine addNode(this)
    !     class(network_t) :: this
    ! end subroutine

    ! function iFactor(this,node) result(res)
    !     class(network_t) :: this
    !     type(fhash_tbl_t) :: node
    !     integer :: res
    !     ! TODO
    ! end function


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
        call this%updateNetworkVoltagesFromCircuit()
    end subroutine



    ! subroutine network_updateBundlesVoltages(this, bundles)
    !     class(network_t) :: this
    !     class(fhash_tbl_t), intent(in) :: bundles
    !     ! TODO: voltages from nw to bundle
    !     ! loop over external nodes
    !     ! get pointer to line attached to node
    !     ! line[.....].v[...] = this%circuit%getNodeVoltage("node")
    ! end subroutine

    ! subroutine network_updateCurrents(this, bundles)
    !     class(network_t) :: this
    !     class(fhash_tbl_t), intent(in) :: bundles
    !     ! TODO: currents from bundle to current
    !     ! loop over external nodes
    !     ! get pointer to line attached to node
    !     ! update current source using current in bundle
    !     ! call this%circuit%updateNodeCurrent(node, current)
    ! end subroutine

    ! subroutine network_computeNWVoltageTerms(this, dt)
    !     class(network_t) :: this
    !     real, intent(in) :: dt
    !     ! TODO
    ! end subroutine 





end module network_mod