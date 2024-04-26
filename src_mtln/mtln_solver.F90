module mtln_solver_mod 

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

    function testPath(parsed, path, err) result(res)
        type(parsed_mtln_t) :: parsed
        character(len=*) :: path
        integer :: res, err
        class(termination_t), allocatable :: t
        write(*,*) 'Inside module:'
        res = 0
        select type (t => parsed%networks(1)%connections(1)%nodes(1)%termination)
        type is (termination_t)
            res = err + 1
        type is (termination_with_source_t)
            write(*,*) '  path_to_excitation:'
            write(*,*) '  ', t%path_to_excitation
            if (t%path_to_excitation /= path) then
                res = err + 1
            end if
        end select
    end function

    function mtlnCtor(parsed) result(res)
        type(parsed_mtln_t) :: parsed
        type(mtln_t) :: res
        integer :: i
        type(preprocess_t) :: pre
        class(termination_t), allocatable :: t

        pre = preprocess(parsed)
        if (size(pre%bundles) == 0) then
            res%number_of_bundles = 0
            return
        end if

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
                this%network_manager%networks(i)%nodes(j)%v = 0.0
                this%network_manager%networks(i)%nodes(j)%i = 0.0
            end do
        end do
    end subroutine

    subroutine mtln_step(this)
        class(mtln_t) :: this
        integer :: i 

        call this%setExternalVoltage()

        call this%advanceBundlesVoltage()
        call this%advanceNWVoltage()
        call this%advanceBundlesCurrent()

        call this%advanceTime()
        call this%updateProbes()

        ! call this%updateExternalCurrent()
    end subroutine

    subroutine step_alone(this)
        class(mtln_t) :: this
        integer :: i 


        call this%advanceBundlesVoltage()
        call this%advanceNWVoltage()
        call this%advanceBundlesCurrent()

        call this%advanceTime()
        call this%updateProbes()

    end subroutine

    subroutine setExternalVoltage(this)
        class(mtln_t) :: this
        integer :: i
        do i = 1, this%number_of_bundles
            call this%bundles(i)%setExternalVoltage()
        end do

    end subroutine

    subroutine updateExternalCurrent(this, currents)
        class(mtln_t) :: this
        ! real, dimension(:,:), intent(inout) :: currents
        real, dimension(:,:,:), intent(inout) :: currents
        integer :: i
        do i = 1, this%number_of_bundles
            call this%bundles(i)%updateExternalCurrent(currents)
            ! call this%bundles(i)%updateExternalCurrent(currents(i,:))
        end do


    end subroutine

    subroutine advanceBundlesVoltage(this)
        class(mtln_t) :: this
        integer :: i
        do i = 1, this%number_of_bundles
            call this%bundles(i)%updateSources(this%time, this%dt)
            call this%bundles(i)%advanceVoltage()
        end do

    end subroutine

    subroutine advanceNWVoltage(this)
        class(mtln_t) :: this
        integer :: i,j
        integer ::b, c, v_idx, i_idx
            
        do i = 1, size(this%network_manager%networks)
            do j = 1, size(this%network_manager%networks(i)%nodes)
                b = this%network_manager%networks(i)%nodes(j)%bundle_number
                c = this%network_manager%networks(i)%nodes(j)%conductor_number
                v_idx = this%network_manager%networks(i)%nodes(j)%v_index
                i_idx = this%network_manager%networks(i)%nodes(j)%i_index

                this%network_manager%networks(i)%nodes(j)%i = this%bundles(b)%i(c, i_idx)
            end do
        end do

        call this%network_manager%advanceVoltage()

        do i = 1, size(this%network_manager%networks)
            do j = 1, size(this%network_manager%networks(i)%nodes)
                b = this%network_manager%networks(i)%nodes(j)%bundle_number
                c = this%network_manager%networks(i)%nodes(j)%conductor_number
                v_idx = this%network_manager%networks(i)%nodes(j)%v_index
                i_idx = this%network_manager%networks(i)%nodes(j)%i_index

                this%bundles(b)%v(c, v_idx) = this%network_manager%networks(i)%nodes(j)%v
            end do
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