module dispersive_mod
    use utils_mod
    ! use utils_mod, only: dotMatmul, entryMatmul, entry, componentSum, add_entries
    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_iter_t, fhash_key_t
    implicit none
    
    type, public :: dispersive_t
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
        procedure :: readResistiveTerm
        procedure :: readInductiveTerm
        procedure :: readResidues
        procedure :: readPoles
        procedure :: findIndex
    end type dispersive_t
    
    interface dispersive_t
        module procedure dispersiveCtor
    end interface

    type, extends(dispersive_t) :: connector_t
    contains
        procedure :: addDispersiveConnector
        procedure :: positionIsEmpty
    end type connector_t

    interface connector_t
        module procedure connectorCtor
    end interface

    type, extends(dispersive_t) :: transfer_impedance_t
    contains
        procedure :: addTransferImpedance
        procedure :: setTransferImpedance
        procedure :: readDirection
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

    function readDirection(this, dict, found) result(res)
        class(transfer_impedance_t) :: this
        type(fhash_tbl_t), intent(in) :: dict
        logical, intent(out), optional :: found
        integer :: stat
        class(*), allocatable :: d
        character(len=:), allocatable :: res
  
        if (present(found)) found = .false.
        call dict%get_raw(key("direction"), d, stat)
        if (stat /= 0) return
  
        select type(d)
         type is (real)
           res = d
           if (present(found)) found = .true.
        end select

    end function

    function readResistiveTerm(this, dict, found) result(res)
        class(dispersive_t) :: this
        type(fhash_tbl_t), intent(in) :: dict
        logical, intent(out), optional :: found
        integer :: stat
        class(*), allocatable :: d
        real :: res
  
        if (present(found)) found = .false.
        call dict%get_raw(key("resistiveTerm"), d, stat)
        if (stat /= 0) return
  
        select type(d)
         type is (real)
           res = d
           if (present(found)) found = .true.
        end select

    end function

    function readResidues(this, dict, found) result(res)
        class(dispersive_t) :: this
        type(fhash_tbl_t), intent(in) :: dict
        logical, intent(out), optional :: found
        integer :: stat
        class(*), allocatable :: d
        complex :: res
  
        if (present(found)) found = .false.
        call dict%get_raw(key("residues"), d, stat)
        if (stat /= 0) return
  
        select type(d)
         type is (real)
           res = d
           if (present(found)) found = .true.
        end select

    end function

    function readPoles(this, dict, found) result(res)
        class(dispersive_t) :: this
        type(fhash_tbl_t), intent(in) :: dict
        logical, intent(out), optional :: found
        integer :: stat
        class(*), allocatable :: d
        complex :: res
  
        if (present(found)) found = .false.
        call dict%get_raw(key("poles"), d, stat)
        if (stat /= 0) return
  
        select type(d)
         type is (real)
           res = d
           if (present(found)) found = .true.
        end select

    end function

    function readInductiveTerm(this, dict, found) result(res)
        class(dispersive_t) :: this
        type(fhash_tbl_t), intent(in) :: dict
        logical, intent(out), optional :: found
        integer :: stat
        class(*), allocatable :: d
        real :: res
  
        if (present(found)) found = .false.
        call dict%get_raw(key("inductiveTerm"), d, stat)
        if (stat /= 0) return
  
        select type(d)
         type is (real)
           res = d
           if (present(found)) found = .true.
        end select

    end function

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

    subroutine addDispersiveConnector(this, position, conductor, model)
        class(connector_t) :: this
        real, dimension(3), intent(in) :: position
        integer, intent(in) :: conductor
        type(fhash_tbl_t), intent(in) :: model
        type(connector_t) :: new_connector
        complex, allocatable, dimension(:) :: poles, residues, q1,q2,q3
        integer :: index, connector_number_of_poles, i

        index = this%findIndex(position)
        if (.not.this%positionIsEmpty(index, conductor))then
            error stop 'Dispersive connector already in conductor at position'
        end if

        poles = this%readPoles(model)
        residues = this%readResidues(model)
        connector_number_of_poles = size(residues)
        q1 = - (residues/poles) * (1.0-(exp(poles*this%dt)-1.0)/(poles*this%dt))
        q2 =   (residues/poles) * (1.0/(poles*this%dt) + exp(poles*this%dt)*(1.0-1.0/(poles*this%dt)))
        q3 =   exp(poles*this%dt)

        if (connector_number_of_poles > this%number_of_poles) then
            new_connector = connector_t(this%number_of_conductors, connector_number_of_poles, this%u, this%dt)
            new_connector%q1(:,:,:,1:this%number_of_poles) = this%q1
            new_connector%q2(:,:,:,1:this%number_of_poles) = this%q2
            new_connector%q3(:,:,:,1:this%number_of_poles) = this%q3
            ! new_connector%phi(:,:,:) = this%phi
            ! new_connector%d(:,:,:) = this%d
            ! new_connector%e(:,:,:) = this%e
            ! new_connector%q3_phi(:,:,:) = 0.0

            new_connector%q1(index, conductor,conductor,:) = q1(:)
            new_connector%q2(index, conductor,conductor,:) = q2(:)
            new_connector%q3(index, conductor,conductor,:) = q3(:)
            ! do i = 1, connector_number_of_poles
            !     new_connector%q1(index, conductor,conductor,i) = new_connector%q1(index, conductor,conductor,i) + q1(i)
            !     new_connector%q2(index, conductor,conductor,i) = new_connector%q2(index, conductor,conductor,i) + q2(i)
            !     new_connector%q3(index, conductor,conductor,i) = new_connector%q3(index, conductor,conductor,i) + q3(i)
            ! end do

            call move_alloc(from=new_connector%q1, to= this%q1)
            call move_alloc(from=new_connector%q2, to= this%q2)
            call move_alloc(from=new_connector%q3, to= this%q3)

        else 
            this%q1(index, conductor, conductor,:) = q1(i)
            this%q2(index, conductor, conductor,:) = q2(i)
            this%q3(index, conductor, conductor,:) = q3(i)
            ! do i = 1, connector_number_of_poles
            !     this%q1(index, conductor, conductor,i) = this%q1(index, conductor, conductor,i) + q1(i)
            !     this%q2(index, conductor, conductor,i) = this%q2(index, conductor, conductor,i) + q2(i)
            !     this%q3(index, conductor, conductor,i) = this%q3(index, conductor, conductor,i) + q3(i)
            ! end do
        end if

        this%d(index, conductor, conductor) = this%readResistiveTerm(model)
        this%e(index, conductor, conductor) = this%readInductiveTerm(model)
        
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
                                                  impedance_model)
        class(transfer_impedance_t) :: this
        integer, intent(in) :: out_level, in_level
        integer, dimension(:), intent(in) :: out_level_conductors, in_level_conductors
        type(fhash_tbl_t), intent(in) :: impedance_model, levels
        type(transfer_impedance_t) :: new_transfer
        complex, allocatable, dimension(:) :: poles, residues, q1,q2,q3
        character(len=:), allocatable :: direction
        integer :: index, connector_number_of_poles, i, j
        integer, dimension(:), allocatable :: range_in, range_out

        poles = this%readPoles(model)
        residues = this%readResidues(model)
        direction = this%readDirection(model)
        connector_number_of_poles = size(residues)
        q1 =   (residues/poles) * (1.0-(exp(poles*this%dt)-1.0)/(poles*this%dt))
        q2 = - (residues/poles) * (1.0/(poles*this%dt) + exp(poles*this%dt)*(1.0-1.0/(poles*this%dt)))
        q3 = - exp(poles*this%dt)

        !todo compute ranges

        ! do i = 1

        this%d(index, conductor, conductor) = this%d(index, conductor, conductor) - this%readResistiveTerm(model)
        this%e(index, conductor, conductor) = this%e(index, conductor, conductor) - this%readInductiveTerm(model)

        if (connector_number_of_poles > this%number_of_poles) then
            new_transfer = connector_t(this%number_of_conductors, connector_number_of_poles, this%u, this%dt)
            new_transfer%q1(:,:,:,1:this%number_of_poles) = this%q1
            new_transfer%q2(:,:,:,1:this%number_of_poles) = this%q2
            new_transfer%q3(:,:,:,1:this%number_of_poles) = this%q3

            new_transfer%q1(index, conductor,conductor,:) = new_transfer%q1(index, conductor,conductor,:) + q1(:)
            new_transfer%q2(index, conductor,conductor,:) = new_transfer%q2(index, conductor,conductor,:) + q2(:)
            new_transfer%q3(index, conductor,conductor,:) = new_transfer%q3(index, conductor,conductor,:) + q3(:)

            call move_alloc(from=new_transfer%q1, to= this%q1)
            call move_alloc(from=new_transfer%q2, to= this%q2)
            call move_alloc(from=new_transfer%q3, to= this%q3)

        else 
            this%q1(index, conductor, conductor,:) = this%q1(index, conductor, conductor,:) + q1(:)
            this%q2(index, conductor, conductor,:) = this%q2(index, conductor, conductor,:) + q2(:)
            this%q3(index, conductor, conductor,:) = this%q3(index, conductor, conductor,:) + q3(:)
        end if

        
        this%q1_sum = sumQComponents(this%q1)
        this%q2_sum = sumQComponents(this%q2)

    end subroutine

    subroutine setTransferImpedance(this)
        class(transfer_impedance_t) :: this


    end subroutine


end module dispersive_mod


