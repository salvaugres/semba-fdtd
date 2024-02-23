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
        ! procedure :: computeTheveninEquivalent

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
                write(*,*) this%networks(i)%nodes(j)%name, " - V: ", this%networks(i)%nodes(j)%v

            end do
        end do
    end subroutine

    subroutine updateNetworkVoltages(this)
        class(network_manager_t) :: this
        integer :: i, j
        real :: is_now, I1, c, step, is_prev, vs_now, vs_prev, r_eq
        real :: k
        do i = 1, size(this%networks)
            do j = 1, this%networks(i)%number_of_nodes
                r_eq = this%circuit%getNodeTheveninR(this%networks(i)%nodes(j)%name)
                vs_now = this%circuit%getNodeTheveninV(this%networks(i)%nodes(j)%name)
                vs_prev = this%networks(i)%nodes(j)%vs_prev

                c = this%networks(i)%nodes(j)%line_c_per_meter
                step = this%networks(i)%nodes(j)%step
                I1 = this%networks(i)%nodes(j)%i
                k = this%dt/(step*c)
                ! vs_now = this%circuit%getNodeVoltage(this%networks(i)%nodes(j)%name) + &
                !          100*this%circuit%getNodeCurrent(this%networks(i)%nodes(j)%name)
                ! if (vs_now /= 0) then 
                !     r_eq = 100.0
                ! end if
                if (index(this%networks(i)%nodes(j)%name, "initial") /= 0) then 

                    this%networks(i)%nodes(j)%v = ((r_eq/k-1.0)*this%networks(i)%nodes(j)%v - &
                                                  2*r_eq*I1 + &
                                                  (vs_now+vs_prev))/(r_eq/k+1.0)
                else 

                    this%networks(i)%nodes(j)%v = ((r_eq/k-1.0)*this%networks(i)%nodes(j)%v + &
                                                  2*r_eq*I1 + &
                                                  (vs_now+vs_prev))/(r_eq/k+1.0)

                end if
                this%networks(i)%nodes(j)%vs_prev = vs_now
                ! write(*,*) this%networks(i)%nodes(j)%name, " - V: ", this%networks(i)%nodes(j)%v
                ! write(*,*) this%networks(i)%nodes(j)%name, " - I: ", this%networks(i)%nodes(j)%i
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

    subroutine updateCircuitVoltagesFromNetwork(this)
        class(network_manager_t) :: this
        integer :: i, j
        do i = 1, size(this%networks)
            do j = 1, this%networks(i)%number_of_nodes
                call this%circuit%updateNodeVoltage(this%networks(i)%nodes(j)%name, &
                                                    this%networks(i)%nodes(j)%v, & 
                                                    this%networks(i)%nodes(j)%i)
            end do
        end do
    end subroutine

    ! subroutine computeTheveninEquivalent(this)
    !     class(network_manager_t) :: this
    !     integer :: i, j
    !     do i = 1, size(this%networks)
    !         do j = 1, this%networks(i)%number_of_nodes
    !             call this%circuit%computeTheveninEquivalent(this%networks(i)%nodes(j)%node_name, &
    !                                                         this%networks(i)%nodes(j)%v, &
    !                                                         this%networks(i)%nodes(j)%v_eq, &
    !                                                         this%networks(i)%nodes(j)%r_eq)
    !         end do
    !     end do
    ! end subroutine

    subroutine network_advanceVoltage(this)
        class(network_manager_t) :: this

        
        call this%circuit%computeTheveninEquivalent()
        call this%updateCircuitVoltagesFromNetwork()
        ! call this%updateCircuitCurrentsFromNetwork()
        call this%circuit%step()
        this%circuit%time = this%circuit%time + this%circuit%dt
        ! call this%updateNetworkVoltagesFromCircuit()
        call this%updateNetworkVoltages()
    end subroutine

end module