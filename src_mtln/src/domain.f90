module domain_mod

    ! use mtlnsolver_mod
    use utils_mod
    implicit none

    type, public :: domain_t
        character (len=:), allocatable :: name
        real, allocatable, dimension(:,:,:) :: lpul, cpul, rpul, gpul
        integer  :: number_of_conductors
        real, allocatable, dimension(:,:) :: u, du
        real, allocatable, dimension(:,:,:) :: duNorm(:,:,:)
        real :: time, dt


    contains
        ! procedure :: add_localized_longitudinal_field
        ! procedure :: addProbe
        ! procedure :: updateLRTerms
        ! procedure :: updateCGTerms
        ! procedure :: get_phase_velocities
        ! procedure :: updateSources
        ! procedure :: advanceVoltage
        ! procedure :: advanceCurrent
        ! procedure :: addTransferImpedance
        ! procedure :: setConnectorTransferImpedance
        

    end type domain_t

    interface domain_t
        module procedure mtldCtor
    end interface domain_t

contains

    function mtldCtor() result(res)
        type(domain_t) :: res
    end function mtldCtor

end module domain_mod