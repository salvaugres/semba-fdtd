module mtl_bundle_mod

    ! use mtlnsolver_mod
    use utils_mod
    use probes_mod
    implicit none

    type, public :: mtl_bundle_t
        character (len=:), allocatable :: name
        real, allocatable, dimension(:,:,:) :: lpul, cpul, rpul, gpul
        integer  :: number_of_conductors
        real, allocatable, dimension(:,:) :: u, du
        real, allocatable, dimension(:,:,:) :: duNorm(:,:,:)
        real :: time, dt
        type(probe_t), allocatable, dimension(:) :: probes


    contains
        ! procedure :: add_localized_longitudinal_field
        procedure :: addProbe
        procedure :: updateLRTerms
        procedure :: updateCGTerms
        ! procedure :: get_phase_velocities
        ! procedure :: updateSources
        ! procedure :: advanceVoltage
        ! procedure :: advanceCurrent
        ! procedure :: addTransferImpedance
        ! procedure :: setConnectorTransferImpedance
        procedure :: isProbeInLine        

    end type mtl_bundle_t

    interface mtl_bundle_t
        module procedure mtldCtor
    end interface

contains

    function mtldCtor() result(res)
        type(mtl_bundle_t) :: res
        allocate(res%probes(0))
    end function

    subroutine addProbe(this, position, probe_type, probe)
        class(mtl_bundle_t) :: this
        real, intent(in), dimension(3) :: position
        character (len=*), intent(in), allocatable :: probe_type
        type(probe_t), intent(inout) :: probe

        if (.not.(this%isProbeInLine(position))) then
            error stop 'Probe position is out of MTL line'
        end if  

        probe = probe_t(position, probe_type, this%dt, this%u)
        this%probes = [this%probes, probe]

    end subroutine 

    function isProbeinLine(this, position) result(res)
        class(mtl_bundle_t) this
        real, intent(in), dimension(3) :: position
        logical :: res
        !TODO
        res = .true.
        
    end function isProbeinLine

    subroutine updateLRTerms(this)
        class(mtl_bundle_t) ::this
        !TODO
    end subroutine

    subroutine updateCGTerms(this)
        class(mtl_bundle_t) ::this
        !TODO
    end subroutine

end module mtl_bundle_mod