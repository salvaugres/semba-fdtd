module mtl_bundle_mod

    ! use mtlnsolver_mod
    use utils_mod
    use probes_mod
    use fhash, only: fhash_tbl_t
    implicit none

    type, public :: mtl_bundle_t
        character (len=:), allocatable :: name
        real, allocatable, dimension(:,:,:) :: lpul, cpul, rpul, gpul
        integer  :: number_of_conductors, number_of_divisions
        real, allocatable, dimension(:,:) :: u, du
        real, allocatable, dimension(:,:) :: v, i
        real, allocatable, dimension(:,:,:) :: duNorm(:,:,:)
        real :: time, dt
        type(probe_t), allocatable, dimension(:) :: probes


    contains
        ! procedure :: add_localized_longitudinal_field
        procedure :: addProbe
        procedure :: updateLRTerms
        procedure :: updateCGTerms
        ! procedure :: get_phase_velocities
        procedure :: updateSources => bundle_updateSources
        procedure :: advanceVoltage => bundle_advanceVoltage
        procedure :: advanceCurrent => bundle_advanceCurrent
        ! procedure :: addTransferImpedance
        ! procedure :: setConnectorTransferImpedance
        procedure :: isProbeInLine        

    end type mtl_bundle_t

    interface mtl_bundle_t
        module procedure mtldCtor
    end interface

contains

    function mtldCtor(levels, name) result(res)
        type(mtl_bundle_t) :: res
        type(fhash_tbl_t), intent(in) :: levels
        character(len=*), intent(in), optional :: name
        real, allocatable, dimension(:,:) :: v, i
        
        res%name = ""
        if (present(name)) then
            res%name = name
        endif   
        res%number_of_conductors = 0
        res%number_of_divisions = 0


        allocate(v(res%number_of_conductors, res%number_of_divisions))
        allocate(i(res%number_of_conductors, res%number_of_divisions - 1))

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

    subroutine bundle_updateSources(this, time, dt)
        class(mtl_bundle_t) ::this
        real, intent(in) :: time, dt
        !TODO
    end subroutine

    subroutine bundle_advanceVoltage(this)
        class(mtl_bundle_t) ::this
        !TODO
    end subroutine

    subroutine bundle_advanceCurrent(this)
        class(mtl_bundle_t) ::this
        !TODO
    end subroutine

end module mtl_bundle_mod