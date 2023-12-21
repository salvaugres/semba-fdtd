module dispersive_mod
    use utils_mod
    ! use utils_mod, only: dotMatmul, entryMatmul, entry, componentSum, add_entries
    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_iter_t, fhash_key_t
    implicit none
    
    type, public :: dispersive_t
        real :: dt
        integer :: number_of_divisions, number_of_conductors, numper_of_poles
        real, allocatable :: u(:,:)
        real, allocatable, dimension(:,:) :: q3_phi
        real, allocatable, dimension(:,:,:) :: d, e, q1_sum, q2_sum
        real, allocatable, dimension(:,:,:) :: phi
        real, allocatable, dimension(:,:,:,:) :: q1, q2, q3
    contains
        ! procedure :: vectorSum
        procedure :: updateQ3Phi
        procedure :: updatePhi
        procedure :: getNumberOfPoles
        procedure :: getResistiveTerm
        procedure :: getInductiveTerm
        procedure :: findIndex
    end type dispersive_t
    
    interface dispersive_t
        module procedure dispersiveCtor
    end interface

    type, extends(dispersive_t) :: connector_t
    contains
        procedure :: addDispersiveConnector
    end type connector_t

    interface connector_t
        module procedure connectorCtor
    end interface

    type, extends(dispersive_t) :: transfer_impedance_t
    contains
        procedure :: addTransferImpedance
        procedure :: setTransferImpedance
    end type transfer_impedance_t

    interface transfer_impedance_t
        module procedure transferImpendaceCtor
    end interface
    



contains
    
    ! elemental function e(this) result(res)
    !     class(entry) :: this
    !     allocate(this%x(1))
    !     this%x=0.0
    ! end subroutine

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

    ! function vectorSum(this, v) result(res)
    !     class(dispersive_t) :: this
    !     real :: res
    !     real, dimension(:) :: v
    !     res = sum(v)
    ! end function 

    function dotmatrixmul(a,b) result(res)
        real, dimension(:,:,:), intent(in) :: a
        real, dimension(:,:), intent(in) :: b
        real, dimension(:), allocatable :: res
        integer :: i,j

        allocate(res(size(a,1)))
        do i = 1, size(a,1)
            res(i) = 0.0
            do j = 1, size(a,2)
                res(i) = res(i) + dot_product(a(i,j,:),b(j,:))
            end do
        end do
    end function

    subroutine updateQ3Phi(this)
        class(dispersive_t) :: this
        integer :: i_div,j,k

        this%q3_phi(:,:) = reshape(source = [(dotmatrixmul(this%q3(i_div, :, :, :), this%phi(i_div, :, :)), i_div = 1, this%number_of_divisions)], &
                                   shape=[this%number_of_divisions, this%number_of_conductors], &
                                   order=[2,1])

        ! do i_div = 1, this%number_of_divisions
        !     this%q3_phi(i_div,:) = dotmatrixmul(this%q1(i_div, :, :, :), this%phi(i_div, :, :))
        ! end do

        ! do k = 1, this%numper_of_poles
        !     do i_div = 1, this%number_of_divisions
        !         this%q3_phi(i_div,:) = this%q3_phi(i_div,:)+matmul(this%q1(i_div, :, :, k), this%phi(i_div, :, k))
        !     end do
        ! end do

        ! this%q3_phi = dotMatmul(this%q3, this%phi)

    end subroutine

    subroutine updatePhi(this, i_prev, i_now)
        class(dispersive_t) :: this
        real, dimension(:,:), intent(in):: i_prev, i_now
        integer :: i_div,k

        do k = 1, this%numper_of_poles
            do i_div = 1, this%number_of_divisions
                this%phi(i_div,:,k) = matmul(this%q1(i_div,:,:,k), i_now(:,i_div)) + &
                                      matmul(this%q2(i_div,:,:,k), i_prev(:,i_div)) + &
                                      matmul(this%q3(i_div,:,:,k), this%phi(i_div,:,k))
            end do
        end do

        ! this%phi = entryMatmul(this%q1, i_now) + &
        !            entryMatmul(this%q2, i_prev)
        !            dotMatmul(this%q3, this%phi)

    end subroutine

    function getNumberOfPoles(this, dict, found) result(res)
        class(dispersive_t) :: this
        type(fhash_tbl_t), intent(in) :: dict
        logical, intent(out), optional :: found
        integer :: stat
        class(*), allocatable :: d
        integer :: res
  
        ! if (present(found)) found = .false.
        ! call dict%get_raw(key("resistiveTerm"), d, stat)
        ! if (stat /= 0) return
  
        ! select type(d)
        !  type is (real)
        !    res = d
        !    if (present(found)) found = .true.
        ! end select
        !TODO
        res  = 0

    end function

    function getResistiveTerm(this, dict, found) result(res)
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

    function getInductiveTerm(this, dict, found) result(res)
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

    subroutine addDispersiveConnector(this, position, conductor, model)
        class(connector_t) :: this
        real, dimension(3), intent(in) :: position
        integer, intent(in) :: conductor
        type(fhash_tbl_t) :: model
        integer :: index, connector_number_of_poles, i
        type(connector_t) :: new_connector
        index = this%findIndex(position)
        !TODO
        !read poles and residues
        !combine into complex p and r
        !assign to q1, q2 and q3
        ! self.q1[index, conductor, conductor] -= (residues / poles) * (
        !     1 - (np.exp(poles * self.dt) - 1) / (poles * self.dt)
        ! )
        ! self.q2[index, conductor, conductor] += (residues / poles) * (
        !     1 / (poles * self.dt)
        !     + np.exp(poles * self.dt) * (1 - 1 / (poles * self.dt))
        ! )
        ! self.q3[index, conductor, conductor] += np.exp(poles * self.dt)

        connector_number_of_poles = this%getNumberOfPoles(model)
        !al añadir un connector, si es necesario amplia la dimensión, y luego añade valores
        !si no, añade el polo/res a la posicion correspondiente        
        if (connector_number_of_poles > this%numper_of_poles) then
            new_connector = connector_t(this%number_of_conductors, connector_number_of_poles, this%u, this%dt)
            new_connector%q1(:,:,:,1:this%numper_of_poles) = this%q1 !copy
            do i = 1, connector_number_of_poles
                new_connector%q1(index, conductor,conductor,i) = new_connector%q1(index, conductor,conductor,i) + 10.0 !assign new values, 10.0 is a placeholder
            end do
            call move_alloc(from=new_connector%q1, to= this%q1)
        else 
            do i = 1, connector_number_of_poles
                this%q1(index, conductor, conductor,i) = this%q1(index, conductor, conductor,i) + 10.0 !the values of the new pole/residue operation
            end do
        end if

        this%d(index, conductor, conductor) = this%d(index, conductor, conductor) + this%getResistiveTerm(model)
        this%e(index, conductor, conductor) = this%e(index, conductor, conductor) + this%getInductiveTerm(model)
        
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

        ! this%q1_sum = componentSum(this%q1)
        ! this%q2_sum = componentSum(this%q2)

    end subroutine

    subroutine setTransferImpedance(this)
        class(transfer_impedance_t) :: this

        ! this%q1_sum = componentSum(this%q1)
        ! this%q2_sum = componentSum(this%q2)

    end subroutine


end module dispersive_mod


