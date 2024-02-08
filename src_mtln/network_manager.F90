module network_manager_mod

    use network_mod
    use circuit_mod
    implicit none 

    type network_manager_t
        type(network_t), dimension(:), allocatable :: networks
        type(circuit_t) :: circuit
        real :: time, dt
    contains
        procedure :: advanceVoltage => network_advanceVoltage
        procedure :: updateCircuitCurrentsFromNetwork
        procedure :: updateNetworkVoltagesFromCircuit
    end type

    interface network_manager_t
        module procedure network_managerCtor
    end interface


contains

    function copy_node_names(networks) result(res)
        type(network_t), dimension(:), intent(in) :: networks
        type(string_t), dimension(:), allocatable :: res
        integer :: i,j
        allocate(res(0))
        do i = 1, size(networks)
            do j = 1, size(networks(i)%nodes)
                res = [res, string_t(networks(i)%nodes(j)%name, len(networks(i)%nodes(j)%name))]
            end do
        end do
        res = [res, string_t("time",4)]
    end function


    function network_managerCtor(networks, description, final_time, dt) result(res)
        type(network_t), dimension(:), intent(in) :: networks
        character(*), dimension(:), intent(in) :: description
        real, intent(in) :: final_time, dt
        type(network_manager_t) :: res

        res%dt = dt
        res%time = 0.0
        res%networks = networks
        call res%circuit%init(copy_node_names(networks))
        res%circuit%dt = dt
        call res%circuit%readInput(description)
        call res%circuit%setStopTimes(final_time, dt)

    end function

    subroutine updateNetworkVoltagesFromCircuit(this)
        class(network_manager_t) :: this
        integer :: i, j
        do i = 1, size(this%networks)
            do j = 1, this%networks(i)%number_of_nodes
                this%networks(i)%nodes(j)%v = this%circuit%getNodeVoltage(this%networks(i)%nodes(j)%name)
            end do
        end do
    end subroutine


    subroutine updateCircuitCurrentsFromNetwork(this)
        class(network_manager_t) :: this
        integer :: i, j
        do i = 1, size(this%networks)
            do j = 1, this%networks(i)%number_of_nodes
                call this%circuit%updateNodeCurrent(this%networks(i)%nodes(j)%name, this%networks(i)%nodes(j)%i)
            end do
        end do
    end subroutine

    subroutine network_advanceVoltage(this)
        class(network_manager_t) :: this

        call this%updateCircuitCurrentsFromNetwork()
        call this%circuit%step()
        this%circuit%time = this%circuit%time + this%circuit%dt
        call this%updateNetworkVoltagesFromCircuit()
    end subroutine

end module