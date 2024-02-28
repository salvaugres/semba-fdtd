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
        procedure :: updateCircuitvoltagesFromNetwork
        procedure :: updateNetworkVoltagesFromCircuit
        procedure :: updateNetworkVoltages
        procedure :: getIsCurrents
    end type

    interface network_manager_t
        module procedure network_managerCtor
    end interface


contains

    function copy_sources_names(networks) result(res)
        type(network_t), dimension(:), intent(in) :: networks
        type(string_t), dimension(:), allocatable :: res
        integer :: i,j
        allocate(res(0))
        do i = 1, size(networks)
            do j = 1, size(networks(i)%nodes)
                res = [res, string_t(networks(i)%nodes(j)%source, len(networks(i)%nodes(j)%source))]
            end do
        end do
    end function

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
        call res%circuit%init(copy_node_names(networks), copy_sources_names(networks))
        res%circuit%dt = dt
        call res%circuit%readInput(description)
        call res%circuit%setStopTimes(final_time, dt)

    end function

    subroutine getIsCurrents(this)
        class(network_manager_t) :: this
        integer :: i,j
        do i = 1, size(this%networks)
            do j = 1, this%networks(i)%number_of_nodes
                this%networks(i)%nodes(j)%is_now = this%circuit%getNodeCurrent(this%networks(i)%nodes(j)%name)
            end do
        end do
    end subroutine

    subroutine updateNetworkVoltagesFromCircuit(this)
        class(network_manager_t) :: this
        integer :: i, j
        do i = 1, size(this%networks)
            do j = 1, this%networks(i)%number_of_nodes
                this%networks(i)%nodes(j)%v = this%circuit%getNodeVoltage(this%networks(i)%nodes(j)%name)
            end do
        end do
    end subroutine

    subroutine updateNetworkVoltages(this)
        class(network_manager_t) :: this
        integer :: i, j
        real :: is_now, is_prev, I1, c, step
        real :: tmp
        do i = 1, size(this%networks)
            do j = 1, this%networks(i)%number_of_nodes
                this%networks(i)%nodes(j)%is_now = this%circuit%getNodeCurrent(this%networks(i)%nodes(j)%name)
                c = this%networks(i)%nodes(j)%line_c_per_meter
                step = this%networks(i)%nodes(j)%step
                ! is_prev = this%networks(i)%nodes(j)%is_prev
                if (index(this%networks(i)%nodes(j)%name, "initial") /= 0) then 
                    is_now = this%networks(i)%nodes(j)%is_now
                    I1 = this%networks(i)%nodes(j)%i
                    tmp = (this%dt/(step*c))*(I1-is_now)
                    this%networks(i)%nodes(j)%v = this%networks(i)%nodes(j)%v - tmp
                    ! this%networks(i)%nodes(j)%v = this%networks(i)%nodes(j)%v - 2*this%dt*I1/(step*c) + &
                                                !   this%dt * (is_now + is_prev)/(step*c)
                else 
                    is_now = this%networks(i)%nodes(j)%is_now
                    I1 = this%networks(i)%nodes(j)%i
                    tmp = (this%dt/(step*c))*(I1-is_now)
                    this%networks(i)%nodes(j)%v = this%networks(i)%nodes(j)%v + tmp
                    ! this%networks(i)%nodes(j)%v = this%networks(i)%nodes(j)%v + 2*this%dt*I1/(step*c) - &
                                                !   this%dt * (is_now + is_prev)/(step*c)

                end if
                write(*,*) this%networks(i)%nodes(j)%name, " - V: ", this%networks(i)%nodes(j)%v
                write(*,*) this%networks(i)%nodes(j)%name, " - I: ", this%networks(i)%nodes(j)%i
                ! this%networks(i)%nodes(j)%is_prev = is_now
            end do
        end do
    end subroutine


    subroutine updateCircuitCurrentsFromNetwork(this)
        class(network_manager_t) :: this
        integer :: i, j
        do i = 1, size(this%networks)
            do j = 1, this%networks(i)%number_of_nodes
                call this%circuit%updateNodeCurrent(this%networks(i)%nodes(j)%name, this%networks(i)%nodes(j)%i)
                ! if (index(this%networks(i)%nodes(j)%name, "initial") /= 0) then 
                !     call this%circuit%updateNodeCurrent(this%networks(i)%nodes(j)%name, this%networks(i)%nodes(j)%i)
                ! else
                !     call this%circuit%updateNodeCurrent(this%networks(i)%nodes(j)%name, this%networks(i)%nodes(j)%i)
                ! end if
            end do
        end do
    end subroutine

    subroutine updateCircuitVoltagesFromNetwork(this)
        class(network_manager_t) :: this
        integer :: i, j
        do i = 1, size(this%networks)
            do j = 1, this%networks(i)%number_of_nodes
                call this%circuit%updateNodeVoltage(this%networks(i)%nodes(j)%name, this%networks(i)%nodes(j)%v)
            end do
        end do
    end subroutine

    subroutine network_advanceVoltage(this)
        class(network_manager_t) :: this

        ! call this%updateCircuitCurrentsFromNetwork()
        call this%updateCircuitVoltagesFromNetwork()
        call this%circuit%step()
        this%circuit%time = this%circuit%time + this%circuit%dt
        ! call this%updateNetworkVoltagesFromCircuit()
        call this%updateNetworkVoltages()
    end subroutine

end module