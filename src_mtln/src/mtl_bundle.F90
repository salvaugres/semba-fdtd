module mtl_bundle_mod

    ! use mtlnsolver_mod
    use utils_mod
    use probes_mod
    use dispersive_mod
    use fhash, only: fhash_tbl_t
    implicit none

    type, public :: mtl_bundle_t
        character (len=:), allocatable :: name
        real, allocatable, dimension(:,:,:) :: lpul, cpul, rpul, gpul
        integer  :: number_of_conductors = 0, number_of_divisions = 0
        real, allocatable, dimension(:,:) :: u, du
        real, allocatable, dimension(:,:) :: v, i
        real, allocatable, dimension(:,:,:) :: du_length(:,:,:)
        real :: time = 0.0, dt = 1e10
        type(probe_t), allocatable, dimension(:) :: probes
        type(transfer_impedance_t) :: transfer_impedance
        type(fhash_tbl_t) :: conductors_in_level

        real, dimension(:,:,:), allocatable :: v_term, i_term
        real, dimension(:,:,:), allocatable :: v_diff, i_diff
        
    contains
        ! procedure :: add_localized_longitudinal_field
        procedure :: mergePULMatrices
        procedure :: addProbe
        procedure :: updateLRTerms
        procedure :: updateCGTerms
        ! procedure :: get_phase_velocities
        procedure :: updateSources => bundle_updateSources
        procedure :: advanceVoltage => bundle_advanceVoltage
        procedure :: advanceCurrent => bundle_advanceCurrent
        procedure :: addTransferImpedance => bundle_addTransferImpedance
        ! procedure :: setConnectorTransferImpedance
        procedure :: isProbeInLine        
        procedure :: setExternalCurrent

    end type mtl_bundle_t

    interface mtl_bundle_t
        module procedure mtldCtor
    end interface

contains

    function mtldCtor(levels, name) result(res)
        type(mtl_bundle_t) :: res
        type(fhash_tbl_t), intent(in) :: levels
        character(len=*), intent(in), optional :: name
        ! real, allocatable, dimension(:,:) :: v, i
        
        res%name = ""
        if (present(name)) then
            res%name = name
        endif   
        allocate(res%probes(0))
        !TODO
        ! call res%mergePULMatrices(levels)

        allocate(res%lpul(res%number_of_divisions, res%number_of_conductors, res%number_of_conductors))
        allocate(res%cpul(res%number_of_divisions + 1, res%number_of_conductors, res%number_of_conductors))
        allocate(res%gpul(res%number_of_divisions + 1, res%number_of_conductors, res%number_of_conductors))
        allocate(res%rpul(res%number_of_divisions, res%number_of_conductors, res%number_of_conductors))
        allocate(res%du_length(res%number_of_divisions, res%number_of_conductors, res%number_of_conductors))
        
        allocate(res%v(res%number_of_conductors, res%number_of_divisions + 1))
        allocate(res%i(res%number_of_conductors, res%number_of_divisions))

        allocate(res%i_term(res%number_of_divisions,res%number_of_conductors,res%number_of_conductors))
        allocate(res%v_diff(res%number_of_divisions,res%number_of_conductors,res%number_of_conductors))

        allocate(res%v_term(res%number_of_divisions + 1,res%number_of_conductors,res%number_of_conductors))
        allocate(res%i_diff(res%number_of_divisions + 1,res%number_of_conductors,res%number_of_conductors))
    
        res%transfer_impedance = transfer_impedance_t(res%number_of_conductors, 0, res%u, res%dt)


    end function

    subroutine mergePULMatrices(this, levels)
        class(mtl_bundle_t) :: this
        type(fhash_tbl_t), intent(in) :: levels
        !TODO
    end subroutine

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

    subroutine bundle_addTransferImpedance(this, out_level, out_level_conductors, &
                                          in_level, in_level_conductors, &
                                          impedance_model)
        class(mtl_bundle_t) :: this
        integer, intent(in) :: out_level, in_level
        integer, dimension(:), intent(in) :: out_level_conductors, in_level_conductors
        type(fhash_tbl_t), intent(in) :: impedance_model

        call this%transfer_impedance%addTransferImpedance(this%conductors_in_level, out_level, out_level_conductors, &
                                                          in_level, in_level_conductors, impedance_model)

    end subroutine

    subroutine updateLRTerms(this)
        class(mtl_bundle_t) ::this
        real, dimension(this%number_of_divisions,this%number_of_conductors,this%number_of_conductors) :: F1, F2, IF1
        integer :: i

        ! do i = 1, this%number_of_divisions
        !     F1(i,:,:) = matmul(this%duNorm(i,:,:), &
        !                        this%lpul(i,:,:)/this%dt + 0.5*this%transfer_impedance%d(i,:,:) + this%transfer_impedance%e(i,:,:)/this%dt + 0.5*this%rpul(i,:,:) + this%transfer_impedance%q1_sum(i,:,:))
        !     F2(i,:,:) = matmul(this%duNorm(i,:,:), &
        !                        this%lpul(i,:,:)/this%dt - 0.5*this%transfer_impedance%d(i,:,:) + this%transfer_impedance%e(i,:,:)/this%dt - 0.5*this%rpul(i,:,:) - this%transfer_impedance%q1_sum(i,:,:))
        !     IF1(i,:,:) = inv(F1(i,:,:))
        !     this%i_term(i,:,:) = matmul(IF1(i,:,:),F2(i,:,:))
        !     this%v_diff(i,:,:) = IF1(i,:,:)
        ! enddo

        F1 = reshape(source=[(matmul( &
            this%du_length(i,:,:), &
            this%lpul(i,:,:)/this%dt + &
                0.5*this%transfer_impedance%d(i,:,:) + &
                this%transfer_impedance%e(i,:,:)/this%dt + &
                0.5*this%rpul(i,:,:) + &
                this%transfer_impedance%q1_sum(i,:,:)), &
            i = 1,this%number_of_divisions)], & 
            shape=[this%number_of_divisions,this%number_of_conductors, this%number_of_conductors], &
            order=[2,3,1])
        F2 = reshape(source=[(matmul( &
            this%du_length(i,:,:), &
            this%lpul(i,:,:)/this%dt - &
            0.5*this%transfer_impedance%d(i,:,:) + &
            this%transfer_impedance%e(i,:,:)/this%dt - &
            0.5*this%rpul(i,:,:) - &
            this%transfer_impedance%q1_sum(i,:,:)), &
            i = 1,this%number_of_divisions)], & 
            shape=[this%number_of_divisions,this%number_of_conductors, this%number_of_conductors], &
            order=[2,3,1])

        IF1 = reshape(source=[(inv(F1(i,:,:)), i = 1, this%number_of_divisions)], &
                      shape=[this%number_of_divisions,this%number_of_conductors, this%number_of_conductors], &
                      order=[2,3,1])
        this%i_term = reshape(&
            source=[(matmul(IF1(i,:,:), F2(i,:,:)), i = 1, this%number_of_divisions)], &
            shape=[this%number_of_divisions,this%number_of_conductors, this%number_of_conductors], &
            order=[2,3,1])
        this%v_diff = IF1

    end subroutine

    subroutine updateCGTerms(this)
        class(mtl_bundle_t) ::this
        real, dimension(this%number_of_divisions + 1,this%number_of_conductors,this%number_of_conductors) :: F1, F2, IF1
        real, dimension(this%number_of_divisions + 1, this%number_of_conductors,this%number_of_conductors) :: extended_du_length
        integer :: i
        
        extended_du_length(1,:,:) = this%du_length(1,:,:)
        do i = 2, this%number_of_divisions
            extended_du_length(i,:,:)= 0.5*(this%du_length(i,:,:)+this%du_length(i-1,:,:))
        end do
        extended_du_length(this%number_of_divisions + 1,:,:) = this%du_length(this%number_of_divisions,:,:)

        F1 = reshape(&
            source=[(matmul(extended_du_length(i,:,:), &
            this%cpul(i,:,:)/this%dt) + 0.5*this%gpul(i,:,:), i = 1, this%number_of_divisions + 1)], &
            shape=[this%number_of_divisions + 1,this%number_of_conductors, this%number_of_conductors], &
            order=[2,3,1])
        F2 = reshape(&
            source=[(matmul(extended_du_length(i,:,:), &
            this%cpul(i,:,:)/this%dt) - 0.5*this%gpul(i,:,:), i = 1, this%number_of_divisions + 1)], &
            shape=[this%number_of_divisions + 1,this%number_of_conductors, this%number_of_conductors], &
            order=[2,3,1])

        IF1 = reshape(&
            source=[(inv(F1(i,:,:)), i = 1, this%number_of_divisions + 1)], &
            shape=[this%number_of_divisions + 1,this%number_of_conductors, this%number_of_conductors], &
            order=[2,3,1])

        this%v_term = reshape(&
            source=[(matmul(IF1(i,:,:), F2(i,:,:)), i = 1, this%number_of_divisions + 1)], &
            shape=[this%number_of_divisions + 1,this%number_of_conductors, this%number_of_conductors], &
            order=[2,3,1])
        this%i_diff = IF1

    end subroutine

    subroutine bundle_updateSources(this, time, dt)
        class(mtl_bundle_t) ::this
        real, intent(in) :: time, dt
        !TODO
    end subroutine

    subroutine bundle_advanceVoltage(this)
        class(mtl_bundle_t) ::this
        integer :: i

        ! do i = 2, this%number_of_divisions
        !     this%v(:, i) = matmul(this%v_term(i,:,:), this%v(:,i)) - &
        !                    matmul(this%i_diff(i,:,:), this%i(:,i) - this%i(:,i-1)  )
        !                    ! + eT?
        ! end do


        !? this%v = reshape(source=[(matmul(this%v_term(i,:,:), this%v(:,i)), i = 2, this%number_of_divisions - 1)], &
        !                  shape = [this%number_of_divisions + 1,this%number_of_conductors, this%number_of_conductors], &
        !                  order = [2,3,1])
    end subroutine

    subroutine bundle_advanceCurrent(this)
        class(mtl_bundle_t) ::this
        real, dimension(:,:), allocatable :: i_prev, i_now
        integer :: i
        ! call this%transfer_impedance%updateQ3Phi()
        ! i_prev = this%i
        ! do i = 1, this%number_of_divisions 
        !     this%i(:,i) = matmul(this%i_term(i,:,:), this%i(:,i)) - &
        !                   matmul(this%v_diff(i,:,:), (this%v(:,i+1) - this%v(:,i))) - &
        !                         !  matmul(0.5*this%du_length(i,:,:), this%el))
        !                   matmul(this%v_diff(i,:,:), matmul(this%du_length(i,:,:), this%transfer_impedance%q3_phi(i,:)))
        ! enddo
        ! !TODO - revisar
        ! i_now = this%i
        ! call this%transfer_impedance%updatePhi(i_prev, i_now)
    end subroutine

    subroutine setExternalCurrent(this, current)
        class(mtl_bundle_t) :: this
        real, dimension(:), intent(in) :: current
        !something like this. The current on the level 0 conductor as an initial condiition
        this%i(1,:) = current

    end subroutine

end module mtl_bundle_mod