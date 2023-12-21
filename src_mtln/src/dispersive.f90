module dispersive_mod
    use utils_mod
    ! use utils_mod, only: dotMatmul, entryMatmul, entry, componentSum, add_entries
    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_iter_t, fhash_key_t
    implicit none
    
    type, public :: dispersive_t
        real :: dt
        integer :: number_of_divisions, number_of_conductors
        real, allocatable :: u(:,:)
        real, allocatable, dimension(:,:) :: q3_phi
        real, allocatable, dimension(:,:,:) :: d, e, q1_sum, q2_sum
        type(entry), allocatable, dimension(:,:) :: phi
        type(entry), allocatable, dimension(:,:,:) :: q1, q2, q3
    contains
        ! procedure :: vectorSum
        procedure :: updateQ3Phi
        procedure :: updatePhi
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

    function dispersiveCtor(number_of_conductors, u, dt) result(res)
        type(dispersive_t) :: res
        integer :: number_of_conductors, number_of_divisions, i
        real :: dt
        real, dimension(:,:) :: u
        res%dt = dt
        res%number_of_conductors = number_of_conductors
        res%number_of_divisions = size(u,1)
        res%u = u
    
        allocate(res%phi(number_of_divisions, number_of_conductors))
        allocate(res%q1 (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%q2 (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%q3 (number_of_divisions, number_of_conductors,number_of_conductors))
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


    subroutine updateQ3Phi(this)
        class(dispersive_t) :: this
        this%q3_phi = dotMatmul(this%q3, this%phi)
    end subroutine

    subroutine updatePhi(this, i_prev, i_now)
        class(dispersive_t) :: this
        real, dimension(:,:), intent(in):: i_prev, i_now
        integer :: i

        this%phi = entryMatmul(this%q1, i_now) + &
                   entryMatmul(this%q2, i_prev)
        !            dotMatmul(this%q3, this%phi)

    end subroutine

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
    
    function connectorCtor(number_of_conductors, u, dt) result(res)
        type(connector_t) :: res
        integer :: number_of_conductors, number_of_divisions
        real :: dt
        real, dimension(:,:) :: u
        res%dispersive_t = dispersiveCtor(number_of_conductors, u, dt)

    end function 

    subroutine addDispersiveConnector(this, position, conductor, model)
        class(connector_t) :: this
        real, dimension(3), intent(in) :: position
        integer, intent(in) :: conductor
        type(fhash_tbl_t) :: model
        integer :: index

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

        this%d(index, conductor, conductor) = this%d(index, conductor, conductor) + this%getResistiveTerm(model)
        this%e(index, conductor, conductor) = this%e(index, conductor, conductor) + this%getInductiveTerm(model)
        
        this%q1_sum = componentSum(this%q1)
        this%q2_sum = componentSum(this%q2)
    end subroutine



    function transferImpendaceCtor(number_of_conductors, u, dt) result(res)
        type(transfer_impedance_t) :: res
        integer :: number_of_conductors, number_of_divisions
        real :: dt
        real, dimension(:,:) :: u
        res%dispersive_t = dispersiveCtor(number_of_conductors, u, dt)
    end function 

    subroutine addTransferImpedance(this, levels, out_level, out_level_conductors, &
                                                  in_level, in_level_conductors, &
                                                  impedance_model)
        class(transfer_impedance_t) :: this
        integer, intent(in) :: out_level, in_level
        integer, dimension(:), intent(in) :: out_level_conductors, in_level_conductors
        type(fhash_tbl_t), intent(in) :: impedance_model, levels

        this%q1_sum = componentSum(this%q1)
        this%q2_sum = componentSum(this%q2)

    end subroutine

    subroutine setTransferImpedance(this)
        class(transfer_impedance_t) :: this

        this%q1_sum = componentSum(this%q1)
        this%q2_sum = componentSum(this%q2)

    end subroutine


end module dispersive_mod


