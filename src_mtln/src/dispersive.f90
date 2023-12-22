module dispersive_mod
    use utils_mod
    ! use utils_mod, only: dotMatmul, entryMatmul, entry, componentSum, add_entries
    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_iter_t, fhash_key_t
    use rational_approximation_mod
    implicit none
    


    type :: dispersive_t
        real :: dt
        integer :: number_of_divisions, number_of_conductors, number_of_poles
        real, allocatable :: u(:,:)
        real, allocatable, dimension(:,:,:) :: d, e
        complex, allocatable, dimension(:,:) :: q3_phi
        complex, allocatable, dimension(:,:,:) :: q1_sum, q2_sum
        complex, allocatable, dimension(:,:,:) :: phi
        complex, allocatable, dimension(:,:,:,:) :: q1, q2, q3
    contains
        procedure :: updateQ3Phi
        procedure :: updatePhi
        procedure, private :: increaseOrder
        procedure, private :: findIndex
    end type dispersive_t
    
    interface dispersive_t
        module procedure dispersiveCtor
    end interface

    type, extends(dispersive_t) :: connector_t
    contains
        procedure :: addDispersiveConnector
        procedure, private :: positionIsEmpty
        procedure, private :: addDispersiveConnectorInConductor
    end type connector_t

    interface connector_t
        module procedure connectorCtor
    end interface

    type, extends(dispersive_t) :: transfer_impedance_t
    contains
        procedure :: addTransferImpedance
        procedure :: setTransferImpedance
        ! procedure, private :: readDirection
        procedure, private :: computeRange
        procedure, private :: addTransferImpedanceInConductors
        procedure, private :: setTransferImpedanceInConductors
    end type transfer_impedance_t

    interface transfer_impedance_t
        module procedure transferImpendaceCtor
    end interface


contains
    

    function dispersiveCtor(number_of_conductors, number_of_poles, u, dt) result(res)
        type(dispersive_t) :: res
        integer :: number_of_conductors, number_of_divisions, number_of_poles
        real :: dt
        real, dimension(:,:) :: u
        res%dt = dt
        res%number_of_conductors = number_of_conductors
        res%number_of_divisions = size(u,1)
        res%u = u
    
        allocate(res%phi(number_of_divisions, number_of_conductors,number_of_poles))
        allocate(res%q1 (number_of_divisions, number_of_conductors,number_of_conductors, number_of_poles))
        allocate(res%q2 (number_of_divisions, number_of_conductors,number_of_conductors, number_of_poles))
        allocate(res%q3 (number_of_divisions, number_of_conductors,number_of_conductors, number_of_poles))
        allocate(res%d  (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%e  (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%q1_sum  (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%q2_sum  (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%q3_phi (number_of_divisions, number_of_conductors))

    end function

    subroutine updateQ3Phi(this)
        class(dispersive_t) :: this
        integer :: i_div,j,k

        this%q3_phi(:,:) = reshape(source = [(dotmatrixmul(this%q3(i_div, :, :, :), this%phi(i_div, :, :)), i_div = 1, this%number_of_divisions)], &
                                   shape=[this%number_of_divisions, this%number_of_conductors], &
                                   order=[2,1])
    end subroutine

    subroutine updatePhi(this, i_prev, i_now)
        class(dispersive_t) :: this
        real, dimension(:,:), intent(in):: i_prev, i_now
        integer :: i_div,k

        do k = 1, this%number_of_poles
            do i_div = 1, this%number_of_divisions
                this%phi(i_div,:,k) = matmul(this%q1(i_div,:,:,k), i_now(:,i_div)) + &
                                      matmul(this%q2(i_div,:,:,k), i_prev(:,i_div)) + &
                                      matmul(this%q3(i_div,:,:,k), this%phi(i_div,:,k))
            end do
        end do

    end subroutine



    function findIndex(this, position) result(res)
        class(dispersive_t) :: this
        real, dimension(3), intent(in) :: position
        integer :: res
        res = 0
        !TODO
    end function
    
    function connectorCtor(number_of_conductors, number_of_poles, u, dt) result(res)
        type(connector_t) :: res
        integer :: number_of_conductors, number_of_divisions, number_of_poles
        real :: dt
        real, dimension(:,:) :: u
        res%dispersive_t = dispersiveCtor(number_of_conductors, number_of_poles, u, dt)
    end function 

    function positionIsEmpty(this, index, conductor) result(res)
        class(connector_t) :: this
        integer, intent(in) :: index, conductor
        logical :: res
        res = .true.
        if ((this%d(index, conductor, conductor) /= 0.0).or.&
            (this%e(index, conductor, conductor) /= 0.0).or.&
            .not.(all(this%q1(index, conductor, conductor,:) == 0.0)) .or.&
            .not.(all(this%q2(index, conductor, conductor,:) == 0.0)) .or.&
            .not.(all(this%q3(index, conductor, conductor,:) == 0.0))) then
                res = .false.
        end if
    end function

    subroutine increaseOrder(this, number_of_poles)
        class(dispersive_t) :: this
        integer, intent(in) :: number_of_poles
        type(dispersive_t) :: new_dispersive

        new_dispersive = dispersive_t(this%number_of_conductors, number_of_poles, this%u, this%dt)
        new_dispersive%q1(:,:,:,1:this%number_of_poles) = this%q1
        new_dispersive%q2(:,:,:,1:this%number_of_poles) = this%q2
        new_dispersive%q3(:,:,:,1:this%number_of_poles) = this%q3
        call move_alloc(from=new_dispersive%q1, to=this%q1)
        call move_alloc(from=new_dispersive%q2, to=this%q2)
        call move_alloc(from=new_dispersive%q3, to=this%q3)


    end subroutine

    subroutine addDispersiveConnector(this, position, conductor, model)
        class(connector_t) :: this
        real, dimension(3), intent(in) :: position
        integer, intent(in) :: conductor
        type(fhash_tbl_t), intent(in) :: model
        complex, allocatable, dimension(:) :: poles, residues, q1,q2,q3
        integer :: index, connector_number_of_poles, i
        type(pol_res_t) :: connector
        index = this%findIndex(position)
        if (.not.this%positionIsEmpty(index, conductor))then
            error stop 'Dispersive connector already in conductor at position'
        end if

        connector = pol_res_t(model, this%dt)

        ! poles = this%readPoles(model)
        ! residues = this%readResidues(model)
        
        ! q1 = - (residues/poles) * (1.0-(exp(poles*this%dt)-1.0)/(poles*this%dt))
        ! q2 =   (residues/poles) * (1.0/(poles*this%dt) + exp(poles*this%dt)*(1.0-1.0/(poles*this%dt)))
        ! q3 =   exp(poles*this%dt)

        if (connector%number_of_poles > this%number_of_poles) call this%increaseOrder(connector%number_of_poles)
        
        call this%addDispersiveConnectorInConductor(index, conductor, connector)
        ! this%q1(index, conductor, conductor,:) = connector%q1(:)
        ! this%q2(index, conductor, conductor,:) = connector%q2(:)
        ! this%q3(index, conductor, conductor,:) = connector%q3(:)
 
        ! this%d(index, conductor, conductor) = connector%r
        ! this%e(index, conductor, conductor) = connector%l
        
        this%q1_sum = sumQComponents(this%q1)
        this%q2_sum = sumQComponents(this%q2)
    end subroutine


    function transferImpendaceCtor(number_of_conductors, number_of_poles, u, dt) result(res)
        type(transfer_impedance_t) :: res
        integer :: number_of_conductors, number_of_divisions, number_of_poles
        real :: dt
        real, dimension(:,:) :: u
        res%dispersive_t = dispersiveCtor(number_of_conductors, number_of_poles, u, dt)
    end function 

    subroutine addTransferImpedance(this, levels, out_level, out_level_conductors, &
                                                  in_level, in_level_conductors, &
                                                  model)
        class(transfer_impedance_t) :: this
        integer, intent(in) :: out_level, in_level
        integer, dimension(:), intent(in) :: out_level_conductors, in_level_conductors
        type(fhash_tbl_t), intent(in) :: model, levels
        complex, allocatable, dimension(:) :: poles, residues, q1,q2,q3
        character(len=:), allocatable :: direction
        integer :: connector_number_of_poles, i, j
        integer, dimension(:), allocatable :: range_in, range_out
        type(pol_res_t) :: connector

        connector = pol_res_t(model, this%dt)

        ! poles = this%readPoles(model)
        ! residues = this%readResidues(model)
        ! direction = this%readDirection(model)
        ! connector_number_of_poles = size(connector%number_of_poles)
        ! q1 =   (residues/poles) * (1.0-(exp(poles*this%dt)-1.0)/(poles*this%dt))
        ! q2 = - (residues/poles) * (1.0/(poles*this%dt) + exp(poles*this%dt)*(1.0-1.0/(poles*this%dt)))
        ! q3 = - exp(poles*this%dt)

        if (connector%number_of_poles > this%number_of_poles) call this%increaseOrder(connector%number_of_poles)

        !todo compute ranges
        range_in =  this%computeRange(in_level, in_level_conductors)        
        range_out = this%computeRange(out_level, out_level_conductors)        
        do i = 1, size(range_out)
            do j = 1, size(range_in)
                if (direction == "inwards" .or. direction == "both") then
                    call this%addTransferImpedanceInConductors(range_in(j), range_out(i), connector)

                    ! this%q1(:,range_in(j), range_out(i),:) = this%q1(:,range_in(j), range_out(i),:) + connector%q1(:)
                    ! this%q2(:,range_in(j), range_out(i),:) = this%q2(:,range_in(j), range_out(i),:) + connector%q2(:)
                    ! this%q3(:,range_in(j), range_out(i),:) = this%q3(:,range_in(j), range_out(i),:) + connector%q3(:)
            
                    ! this%d(:, range_in(j), range_out(i)) = this%d(:, range_in(j), range_out(i)) - connector%r
                    ! this%e(:, range_in(j), range_out(i)) = this%e(:, range_in(j), range_out(i)) - connector%l
                else if (direction == "outwards" .or. direction == "both") then
                    call this%addTransferImpedanceInConductors(range_out(i), range_in(j), connector)
                    ! this%q1(:,range_out(i), range_in(j),:) = this%q1(:,range_out(i), range_in(j),:) + connector%q1(:)
                    ! this%q2(:,range_out(i), range_in(j),:) = this%q2(:,range_out(i), range_in(j),:) + connector%q2(:)
                    ! this%q3(:,range_out(i), range_in(j),:) = this%q3(:,range_out(i), range_in(j),:) + connector%q3(:)

                    ! this%d(:, range_out(i), range_in(j)) = this%d(:, range_out(i), range_in(j)) - connector%r
                    ! this%e(:, range_out(i), range_in(j)) = this%e(:, range_out(i), range_in(j)) - connector%l
                end if
            end do
        end do

        this%q1_sum = sumQComponents(this%q1)
        this%q2_sum = sumQComponents(this%q2)

        
    end subroutine
    
    function computeRange(this, level, conductors) result(res)
        class(transfer_impedance_t) :: this
        integer, intent(in) :: level
        integer, dimension(:), intent(in) :: conductors
        integer, dimension(:), allocatable :: res
        !TODO res
    end function

    subroutine setTransferImpedance(this, levels, out_level, out_level_conductors, &
                                    in_level, in_level_conductors, index, &
                                    model)
        class(transfer_impedance_t) :: this
        integer, intent(in) :: out_level, in_level
        integer, dimension(:), intent(in) :: out_level_conductors, in_level_conductors
        integer, intent(in) :: index
        type(fhash_tbl_t), intent(in) :: model, levels

        ! complex, allocatable, dimension(:) :: poles, residues, q1,q2,q3
        ! character(len=:), allocatable :: direction
        integer :: i, j
        integer, dimension(:), allocatable :: range_in, range_out

        type(pol_res_t) :: connector

        ! poles = this%readPoles(model)
        ! residues = this%readResidues(model)
        ! direction = this%readDirection(model)
        ! q1 =   (residues/poles) * (1.0-(exp(poles*this%dt)-1.0)/(poles*this%dt))
        ! q2 = - (residues/poles) * (1.0/(poles*this%dt) + exp(poles*this%dt)*(1.0-1.0/(poles*this%dt)))
        ! q3 = - exp(poles*this%dt)

        if (connector%number_of_poles > this%number_of_poles) call this%increaseOrder(connector%number_of_poles)

        !todo compute ranges
        range_in =  this%computeRange(in_level, in_level_conductors)        
        range_out = this%computeRange(out_level, out_level_conductors)        
        do i = 1, size(range_out)
            do j = 1, size(range_in)
                if (connector%direction == "inwards" .or. connector%direction == "both") then
                    call this%setTransferImpedanceInConductors(index, range_in(j), range_out(i), connector)
                    ! this%q1(index,range_in(j), range_out(i),:) = connector%q1(:)
                    ! this%q2(index,range_in(j), range_out(i),:) = connector%q2(:)
                    ! this%q3(index,range_in(j), range_out(i),:) = connector%q3(:)
            
                    ! this%d(index, range_in(j), range_out(i)) = - connector%r
                    ! this%e(index, range_in(j), range_out(i)) = - connector%l
                else if (connector%direction == "outwards" .or. connector%direction == "both") then
                    call this%setTransferImpedanceInConductors(index, range_out(i), range_in(j), connector)
                    ! this%q1(index,range_out(i), range_in(j),:) = connector%q1(:)
                    ! this%q2(index,range_out(i), range_in(j),:) = connector%q2(:)
                    ! this%q3(index,range_out(i), range_in(j),:) = connector%q3(:)

                    ! this%d(index, range_out(i), range_in(j)) = - connector%r
                    ! this%e(index, range_out(i), range_in(j)) = - connector%l
                end if
            end do
        end do

        this%q1_sum = sumQComponents(this%q1)
        this%q2_sum = sumQComponents(this%q2)
    end subroutine

    subroutine setTransferImpedanceInConductors(this, index, conductor_1, conductor_2, connector)
        class(transfer_impedance_t) :: this
        integer, intent(in) :: index, conductor_1, conductor_2
        type(pol_res_t), intent(in) :: connector

        this%q1(index, conductor_1, conductor_2,:) = connector%q1(:)
        this%q2(index, conductor_1, conductor_2,:) = connector%q2(:)
        this%q3(index, conductor_1, conductor_2,:) = connector%q3(:)

        this%d(index, conductor_1, conductor_2) = - connector%r
        this%e(index, conductor_1, conductor_2) = - connector%l
    end subroutine

    subroutine addTransferImpedanceInConductors(this, index_1, index_2, connector)
        class(transfer_impedance_t) :: this
        integer, intent(in) :: index_1, index_2
        type(pol_res_t), intent(in) :: connector
        integer :: i

        do i = 1, this%number_of_divisions
            this%q1(i,index_1, index_2,:) = this%q1(i,index_1, index_2,:) + connector%q1(:)
            this%q2(i,index_1, index_2,:) = this%q2(i,index_1, index_2,:) + connector%q2(:)
            this%q3(i,index_1, index_2,:) = this%q3(i,index_1, index_2,:) + connector%q3(:)

            this%d(i, index_1, index_2) = this%d(i, index_1, index_2) - connector%r
            this%e(i, index_1, index_2) = this%e(i, index_1, index_2) - connector%l
        end do
    end subroutine

    subroutine addDispersiveConnectorInConductor(this, index, conductor, connector)
        class(connector_t) :: this
        integer, intent(in) :: index, conductor
        type(pol_res_t), intent(in) :: connector

        this%q1(index, conductor, conductor,:) = connector%q1(:)
        this%q2(index, conductor, conductor,:) = connector%q2(:)
        this%q3(index, conductor, conductor,:) = connector%q3(:)

        this%d(index, conductor, conductor) = - connector%r
        this%e(index, conductor, conductor) = - connector%l
    end subroutine


end module dispersive_mod


