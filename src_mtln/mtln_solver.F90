module mtln_solver_mod 

    use types_mod, only: bundle_iter_t
    use mtl_bundle_mod
    use network_manager_mod
    use preprocess_mod
    implicit none


    type, public :: mtln_t
        real :: time, dt
        type(mtl_bundle_t), allocatable, dimension(:) :: bundles
        type(network_manager_t) :: network_manager
        type(probe_t), allocatable, dimension(:) :: probes
        integer :: number_of_bundles
    contains

        procedure :: updateBundlesTimeStep
        procedure :: updatePULTerms
        procedure :: initNodes
        procedure :: getTimeRange
        procedure :: updateProbes
        procedure :: advanceNWVoltage
        procedure :: advanceBundlesVoltage
        procedure :: advanceBundlesCurrent
        procedure :: advanceTime
        procedure :: step => mtln_step
        procedure :: step_alone
        procedure :: setExternalVoltage
        procedure :: updateExternalCurrent
        
        procedure :: runUntil
    end type mtln_t


    interface mtln_t
        module procedure mtlnCtor
    end interface

contains

    function mtlnCtor(parsed) result(res)
        type(parsed_t) :: parsed
        type(mtln_t) :: res
        integer :: i
        type(preprocess_t) :: pre

        pre = preprocess(parsed)
        res%dt = pre%dt
        res%time  = 0.0
        
        res%bundles = pre%bundles
        res%network_manager = pre%network_manager
        res%number_of_bundles = size(res%bundles)
        res%probes = pre%probes
        call res%updateBundlesTimeStep(res%dt)
        call res%updatePULTerms(res%getTimeRange(pre%final_time))
        call res%initNodes()
    end function

    subroutine initNodes(this)
        class(mtln_t) :: this
        integer :: i,j
        do i = 1, size(this%network_manager%networks)
            do j = 1, size(this%network_manager%networks(i)%nodes)
                this%network_manager%networks(i)%nodes(j)%values%v = 0.0
                this%network_manager%networks(i)%nodes(j)%values%i = 0.0
            end do
        end do
    end subroutine

    subroutine mtln_step(this, currents, voltages)
        class(mtln_t) :: this
        real, dimension(:,:), intent(in out) :: currents
        real, dimension(:,:), intent(in) :: voltages
        integer :: i 

        call this%setExternalVoltage(voltages)

        call this%advanceBundlesVoltage()
        call this%advanceNWVoltage()
        call this%advanceBundlesCurrent()
        ! call this%updateNWCurrent()

        call this%advanceTime()
        call this%updateProbes()

        call this%updateExternalCurrent(currents)
    end subroutine

    subroutine step_alone(this)
        class(mtln_t) :: this
        integer :: i 


        call this%advanceBundlesVoltage()
        call this%advanceNWVoltage()
        call this%advanceBundlesCurrent()
        ! call this%updateNWCurrent()

        call this%advanceTime()
        call this%updateProbes()

    end subroutine

    subroutine setExternalVoltage(this, voltages)
        class(mtln_t) :: this
        real, dimension(:,:), intent(in) :: voltages
        integer :: i
        do i = 1, this%number_of_bundles
            call this%bundles(i)%setExternalVoltage(voltages(i,:))
        end do

    end subroutine

    subroutine updateExternalCurrent(this, currents)
        class(mtln_t) :: this
        real, dimension(:,:), intent(inout) :: currents
        integer :: i
        do i = 1, this%number_of_bundles
            call this%bundles(i)%updateExternalCurrent(currents(i,:))
        end do


    end subroutine

    subroutine advanceBundlesVoltage(this)
        class(mtln_t) :: this
        integer :: i
        do i = 1, this%number_of_bundles
            call this%bundles(i)%updateSources(this%time, this%dt)
            call this%bundles(i)%advanceVoltage()

            ! this%bundles(i)%v_initial(:) = this%bundles(i)%v(:,1)
            ! this%bundles(i)%v_end(:)     = this%bundles(i)%v(:,ubound(this%bundles(i)%v, 2))

            ! this%bundles(i)%i_initial(:) = this%bundles(i)%i(:,1)
            ! this%bundles(i)%i_end(:)     = this%bundles(i)%i(:,ubound(this%bundles(i)%i, 2))

        end do
    end subroutine

    subroutine advanceNWVoltage(this)
        class(mtln_t) :: this
        integer :: i
        call this%network_manager%advanceVoltage()
        do i = 1, this%number_of_bundles

            ! this%bundles(i)%v(:,1) = this%bundles(i)%v_initial(:)
            ! this%bundles(i)%v(:,ubound(this%bundles(i)%v, 2)) = this%bundles(i)%v_end(:)

            ! this%bundles(i)%i(:,1) = this%bundles(i)%i_initial(:)
            ! this%bundles(i)%i(:,ubound(this%bundles(i)%i, 2)) = this%bundles(i)%i_end(:)

        end do


    end subroutine

    subroutine advanceBundlesCurrent(this)
        class(mtln_t) :: this
        integer :: i
        do i = 1, this%number_of_bundles
            call this%bundles(i)%advanceCurrent()

        end do
    end subroutine

    subroutine advanceTime(this)
        class(mtln_t) :: this
        this%time = this%time + this%dt
    end subroutine

    subroutine updateProbes(this)
        class(mtln_t) :: this
        integer :: i, j
        do i = 1, this%number_of_bundles
            do j = 1, size(this%bundles(i)%probes)
                call this%bundles(i)%probes(j)%update(this%time, this%bundles(i)%v, this%bundles(i)%i)
            end do 
        end do
    end subroutine

    function getTimeRange(this, final_time) result(res)
        class(mtln_t) :: this
        real, intent(in) :: final_time
        integer :: res
        res =  floor(final_time / this%dt)
    end function

    subroutine updateBundlesTimeStep(this, dt)
        class(mtln_t) :: this
        real :: dt
        integer :: i
        do i = 1, this%number_of_bundles
            this%bundles(i)%dt = dt
        end do
    end subroutine

    subroutine updatePULTerms(this, n_time_steps)
        class(mtln_t) :: this
        integer, intent(in) :: n_time_steps
        integer :: i, j 
        do i = 1, this%number_of_bundles
            call this%bundles(i)%updateLRTerms()
            call this%bundles(i)%updateCGTerms()
            do j = 1, size(this%bundles(i)%probes)
                call this%bundles(i)%probes(j)%resizeFrames(n_time_steps, this%bundles(i)%number_of_conductors)
            end do
        end do
   
    end subroutine


    subroutine runUntil(this, final_time)
        class(mtln_t) :: this
        real, intent(in) :: final_time
        integer :: i

        do i = 1, this%getTimeRange(final_time)
            call this%advanceBundlesVoltage()
            call this%advanceNWVoltage()
            call this%advanceBundlesCurrent()
            call this%advanceTime()
            call this%updateProbes()
        end do

    end subroutine


end module mtln_solver_mod