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
        real :: line_c_per_meter
    end type


    type, public :: network_t
        integer :: number_of_nodes = 0
        type(node_t), dimension(:), allocatable :: nodes
        character(50), dimension(:), allocatable :: description
        type(circuit_t), pointer :: circuit
    contains

    ! procedure, private :: updateNetworkVoltagesFromCircuit
    ! procedure, private :: updateCircuitCurrentsFromNetwork

    ! procedure :: advanceVoltage => network_advanceVoltage
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

    function networkCtor(nodes, description) result(res)
    ! function networkCtor(nodes, description, final_time, dt) result(res)
        type(node_t), dimension(:), intent(in) :: nodes
        character(*), dimension(:), intent(in) :: description
        type(string_t), dimension(:), allocatable :: names
        ! real :: final_time, dt
        integer :: i
        type(network_t) :: res

        res%nodes = nodes
        res%description = description
        res%number_of_nodes = size(nodes)

        allocate(names(size(nodes)))
        do i = 1, size(nodes)
            names(i)%name = nodes(i)%name
            names(i)%length = len(nodes(i)%name)
        end do

        ! call res%circuit%init(names)
        ! call res%circuit%setStopTimes(final_time, dt)
        ! call res%circuit%readInput(res%description)

    end function


    ! subroutine updateNetworkVoltagesFromCircuit(this)
    !     class(network_t) :: this
    !     integer :: i
    !     do i = 1, this%number_of_nodes
    !         this%nodes(i)%v = this%circuit%getNodeVoltage(this%nodes(i)%name)
    !     end do
    ! end subroutine

    ! subroutine updateCircuitCurrentsFromNetwork(this)
    !     class(network_t) :: this
    !     integer :: i
    !     do i = 1, this%number_of_nodes
    !         call this%circuit%updateNodeCurrent(this%nodes(i)%name, this%nodes(i)%i)
    !     end do
    ! end subroutine

    ! subroutine network_advanceVoltage(this, dt)
    !     class(network_t) :: this
    !     real, intent(in) :: dt
    !     call this%updateCircuitCurrentsFromNetwork()
    !     call this%circuit%step()
    !     this%circuit%time = this%circuit%time + this%circuit%dt
    !     call this%updateNetworkVoltagesFromCircuit()
    ! end subroutine

end module network_mod