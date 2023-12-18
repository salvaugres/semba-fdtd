module dispersive_mod
    implicit none
    
    type, public :: entry
        real :: x
    !     real :: sum

    ! contains
    !     procedure :: e
    end type entry

    type, public :: dispersive_t
        real :: dt
        integer :: number_of_divisions, number_of_conductors
        real, allocatable, dimension(:,:) :: u
        type(entry(:)), allocatable, dimension(:,:) :: phi, q3_phi
        ! type(entry), allocatable, dimension(:,:) :: phi, q3_phi
        type(entry), allocatable, dimension(:,:,:) :: d, e, q1, q2, q3, q1_sum, q2_sum
    contains
        procedure :: vectorSum
        procedure :: updateQ3Phi
        procedure :: updatePhi
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

    function dispersiveCtor(number_of_conductors, number_of_divisions, u, dt) result(res)
        type(dispersive_t) :: res
        integer :: number_of_conductors, number_of_divisions
        real :: dt
        real, dimension(:,:) :: u
        res%dt = dt
        res%number_of_conductors = number_of_conductors
        res%number_of_divisions = number_of_divisions
        res%u = u
        allocate(res%phi(number_of_divisions, number_of_conductors))
        ! res%phi(0,0)%x = 1.0
        ! res%phi(0,0)%x = [res%phi(0,0)%x,1.0]
        ! res%phi(:,:)%
        allocate(res%q1 (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%q2 (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%q3 (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%d  (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%e  (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%q1_sum  (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%q2_sum  (number_of_divisions, number_of_conductors,number_of_conductors))
        allocate(res%q3_phi (number_of_divisions, number_of_conductors))
    end function

    function vectorSum(this, v) result(res)
        class(dispersive_t) :: this
        real :: res
        real, dimension(:) :: v
        res = sum(v)
    end function 


    subroutine updateQ3Phi(this)
        class(dispersive_t) :: this
        integer :: i
        type(entry), dimension(:,:), allocatable :: res
        ! do i = 1, this%number_of_divisions
        !     res = dot_product(this%q3(i,:,:), this%phi(i,:))
        !     this%q3_phi(i,:) = matmul(this%q3(i), this%phi(i))
        ! end do

    end subroutine

    subroutine updatePhi(this)
        class(dispersive_t) :: this
    end subroutine

    
    function connectorCtor(number_of_conductors, number_of_divisions, u, dt) result(res)
        type(connector_t) :: res
        integer :: number_of_conductors, number_of_divisions
        real :: dt
        real, dimension(:,:) :: u
        res%dispersive_t = dispersiveCtor(number_of_conductors, number_of_divisions, u, dt)

    end function 

    subroutine addDispersiveConnector(this)
        class(connector_t) :: this
    end subroutine



    function transferImpendaceCtor(number_of_conductors, number_of_divisions, u, dt) result(res)
        type(connector_t) :: res
        integer :: number_of_conductors, number_of_divisions
        real :: dt
        real, dimension(:,:) :: u
        res%dispersive_t = dispersiveCtor(number_of_conductors, number_of_divisions, u, dt)
    end function 

    subroutine addTransferImpedance(this)
        class(transfer_impedance_t) :: this
    end subroutine

    subroutine setTransferImpedance(this)
        class(transfer_impedance_t) :: this
    end subroutine


end module dispersive_mod


