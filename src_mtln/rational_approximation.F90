module rational_approximation_mod
    ! use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_iter_t, fhash_key_t
    use mtln_types_mod
    implicit none

    type :: pol_res_t
        complex, allocatable, dimension(:) :: q1,q2,q3
        real :: r, l
        integer :: number_of_poles
        type(transfer_impedance_per_meter_t) :: model
        character(len=:), allocatable :: direction
    end type

    interface pol_res_t
        module procedure pol_resCtor
    end interface

contains

    function pol_resCtor(model, dt) result(res)
        type(transfer_impedance_per_meter_t), intent(in) :: model
        real, intent(in) :: dt
        type(pol_res_t) :: res

        res%r = model%resistive_term
        res%l = model%inductive_term

        res%number_of_poles = size(model%poles)
        allocate(res%q1(res%number_of_poles), res%q2(res%number_of_poles), res%q3(res%number_of_poles))
        if (res%number_of_poles /= 0) then
            res%q1 =   (model%residues/model%poles) * (1.0-(exp(model%poles*dt)-1.0)/(model%poles*dt))
            res%q2 = - (model%residues/model%poles) * (1.0/(model%poles*dt) + exp(model%poles*dt)*(1.0-1.0/(model%poles*dt)))
            res%q3 = - exp(model%poles*dt)
        end if
        res%direction = res%direction
    end function


end module