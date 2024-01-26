module rational_approximation_mod
    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_iter_t, fhash_key_t
    implicit none

    type :: pol_res_t
        complex, allocatable, dimension(:) :: q1,q2,q3
        real :: r, l
        integer :: number_of_poles
        type(fhash_tbl_t) :: model
        character(len=:), allocatable :: direction
    contains
        procedure, private :: readResistiveTerm
        procedure, private :: readInductiveTerm
        procedure, private :: readResidues
        procedure, private :: readPoles
        procedure, private :: readDirection
    end type

    interface pol_res_t
        module procedure pol_resCtor
    end interface

contains

    function pol_resCtor(model, dt) result(res)
        type(fhash_tbl_t), intent(in) :: model
        real, intent(in) :: dt
        type(pol_res_t) :: res
        complex, allocatable, dimension(:) :: poles, residues

        res%model = model
        res%r = res%readResistiveTerm()
        res%l = res%readInductiveTerm()
        
        poles = res%readPoles()
        residues = res%readResidues()

        res%number_of_poles = size(poles)

        res%q1 =   (residues/poles) * (1.0-(exp(poles*dt)-1.0)/(poles*dt))
        res%q2 = - (residues/poles) * (1.0/(poles*dt) + exp(poles*dt)*(1.0-1.0/(poles*dt)))
        res%q3 = - exp(poles*dt)

        res%direction = res%readDirection()
    end function

    function readDirection(this, found) result(res)
        class(pol_res_t) :: this
        logical, intent(out), optional :: found
        integer :: stat
        class(*), allocatable :: d
        character(len=:), allocatable :: res

        if (present(found)) found = .false.
        call this%model%get_raw(key("direction"), d, stat)
        if (stat /= 0) return

        select type(d)
        type is (character(len=*))
        res = d
        if (present(found)) found = .true.
        end select

    end function

    function readResistiveTerm(this, found) result(res)
        class(pol_res_t) :: this
        logical, intent(out), optional :: found
        integer :: stat
        class(*), allocatable :: d
        real :: res

        if (present(found)) found = .false.
        call this%model%get_raw(key("resistiveTerm"), d, stat)
        if (stat /= 0) return

        select type(d)
        type is (real)
        res = d
        if (present(found)) found = .true.
        end select

    end function

    function readResidues(this, found) result(res)
        class(pol_res_t) :: this
        logical, intent(out), optional :: found
        integer :: stat
        class(*), allocatable :: d
        complex :: res

        if (present(found)) found = .false.
        call this%model%get_raw(key("residues"), d, stat)
        if (stat /= 0) return

        select type(d)
        type is (real)
        res = d
        if (present(found)) found = .true.
        end select

    end function

    function readPoles(this, found) result(res)
        class(pol_res_t) :: this
        logical, intent(out), optional :: found
        integer :: stat
        class(*), allocatable :: d
        complex :: res

        if (present(found)) found = .false.
        call this%model%get_raw(key("poles"), d, stat)
        if (stat /= 0) return

        select type(d)
        type is (real)
        res = d
        if (present(found)) found = .true.
        end select

    end function

    function readInductiveTerm(this, found) result(res)
        class(pol_res_t) :: this
        logical, intent(out), optional :: found
        integer :: stat
        class(*), allocatable :: d
        real :: res

        if (present(found)) found = .false.
        call this%model%get_raw(key("inductiveTerm"), d, stat)
        if (stat /= 0) return

        select type(d)
        type is (real)
        res = d
        if (present(found)) found = .true.
        end select

    end function


end module