module mtln_solver_mod 

    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    use types_mod, only: bundle_iter_t
    use mtl_bundle_mod
    use network_bundle_mod
    use preprocess_mod
    implicit none


    type, public :: mtln_t
        real :: time, dt
        type(fhash_tbl_t) :: bundles ! dict{name:bundle} of MLTD
        type(network_bundle_t), allocatable, dimension(:) :: networks !lista de NetworkD, no de network
        character(len=:), allocatable, dimension(:) :: names
    contains

        procedure :: addBundle => mtln_setBundle
        procedure :: getBundle => mtln_getBundle
        procedure :: findBundle => mtln_findBundle
        procedure :: updateBundlesTimeStep
        procedure :: updatePULTerms
        procedure :: addNetwork
        procedure :: getTimeRange
        ! procedure :: computeNWVoltageTerms
        procedure :: runUntil
        procedure :: updateProbes
        procedure :: updateNWCurrent
        procedure :: advanceNWVoltage
        procedure :: advanceBundlesVoltage
        procedure :: advanceBundlesCurrent
        procedure :: advanceTime
        procedure :: step => mtln_step


    end type mtln_t


    interface mtln_t
        module procedure mtlnCtor
    end interface

contains

    function mtlnCtor(parsed) result(res)
        type(parsed_t) :: parsed
        type(mtln_t) :: res
        integer :: i
        type(preprocess_t) :: preprocess

        preprocess = preprocessCtor(parsed)
        res%dt = 1e10
        res%time  = 0.0
        allocate(res%networks(0))
        do i = 1, size(preprocess%bundles)
            call res%addBundle(preprocess%bundles(i)%name, preprocess%bundles(i))
        end do
        do i = 1, size(preprocess%networks)
            call res%addNetwork(preprocess%networks(i))
        end do

    end function
    ! function mtlnCtor(bundles, networks) result(res)
    !     type(mtln_t) :: res
    !     class(mtl_bundle_t), dimension(:) :: bundles
    !     class(network_bundle_t), dimension(:) :: networks
    !     integer :: i

    !     res%dt = 1e10
    !     res%time  = 0.0
    !     allocate(res%networks(0))
    !     do i = 1, size(bundles)
    !         call res%addBundle(bundles(i)%name, bundles(i))
    !     end do
    !     do i = 1, size(networks)
    !         call res%addNetwork(networks(i))
    !     end do

    ! end function

    subroutine mtln_step(this)
        class(mtln_t) :: this

        call this%advanceBundlesVoltage()
        call this%advanceNWVoltage()
        call this%advanceBundlesCurrent()
        call this%updateNWCurrent()

        call this%advanceTime()
        call this%updateProbes()

    end subroutine

    subroutine advanceBundlesVoltage(this)
        class(mtln_t) :: this
        type(bundle_iter_t) :: iter
        class(fhash_key_t), allocatable :: name
        class(mtl_bundle_t), pointer :: bundle
        iter = bundle_iter_t(this%bundles)
        do while(iter%findNext(name,bundle))
            call bundle%updateSources(this%time, this%dt)
            call bundle%advanceVoltage()
        enddo
    end subroutine

    subroutine advanceNWVoltage(this)
        class(mtln_t) :: this
        integer :: i
        do i = 1, size(this%networks)
            ! call this%networks(i)%updateSources(this%time, this%dt)
            call this%networks(i)%advanceVoltage(this%dt)
            call this%networks(i)%updateBundlesVoltages(this%bundles)
        end do
    end subroutine

    subroutine advanceBundlesCurrent(this)
        class(mtln_t) :: this
        type(bundle_iter_t) :: iter
        class(fhash_key_t), allocatable :: name
        class(mtl_bundle_t), pointer :: bundle
        iter = bundle_iter_t(this%bundles)
        do while(iter%findNext(name,bundle))
            call bundle%advanceCurrent()
        enddo
    end subroutine advanceBundlesCurrent

    subroutine updateNWCurrent(this)
        class(mtln_t) :: this
        integer :: i
        do i = 1, size(this%networks)
            call this%networks(i)%updateCurrents(this%bundles)
        end do
    end subroutine

    subroutine advanceTime(this)
        class(mtln_t) :: this
        this%time = this%time + this%dt
    end subroutine

    subroutine updateProbes(this)
        class(mtln_t) :: this
        type(bundle_iter_t) :: iter
        class(fhash_key_t), allocatable :: name
        class(mtl_bundle_t), pointer :: bundle
        integer :: i
        iter = bundle_iter_t(this%bundles)
        do while(iter%findNext(name,bundle))
            do i = 1, size(bundle%probes)
                call bundle%probes(i)%update(this%time, bundle%v, bundle%i)
            end do
        end do

    end subroutine


    subroutine addNetwork(this, network)
        class(mtln_t) :: this
        class(network_bundle_t) :: network
        this%networks = [this%networks, network]
        call network%updateIndexNumbers(this%bundles)
    end subroutine


    function getTimeRange(this, final_time) result(res)
        class(mtln_t) :: this
        real, intent(in) :: final_time
        integer :: res
        integer :: i
        res =  floor(final_time / this%dt)
    end function

    ! subroutine computeNWVoltageTerms(this)
    !     class(mtln_t) :: this
    !     integer :: i
    !     do i = 1, size(this%networks)
    !         call this%networks(i)%computeNWVoltageTerms(this%dt)
    !     end do
    ! end subroutine

    subroutine updateBundlesTimeStep(this, dt)
        class(mtln_t) :: this
        type(bundle_iter_t) :: iter
        class(fhash_key_t), allocatable :: name
        class(mtl_bundle_t), pointer :: bundle
        real :: dt

        iter = bundle_iter_t(this%bundles)
        do while(iter%findNext(name,bundle))
            bundle%dt = dt
        end do

    end subroutine

    subroutine updatePULTerms(this, n_time_steps)
        class(mtln_t) :: this
        integer, intent(in) :: n_time_steps
        type(bundle_iter_t) :: iter
        class(fhash_key_t), allocatable :: name
        class(mtl_bundle_t), pointer :: bundle
        integer :: i
        iter = bundle_iter_t(this%bundles)
        do while(iter%findNext(name,bundle))
                call bundle%updateLRTerms()
                call bundle%updateCGTerms()
                do i = 1, size(bundle%probes)
                    call bundle%probes(i)%resizeFrames(n_time_steps, bundle%number_of_conductors)
                enddo
        end do
   
    end subroutine

    subroutine runUntil(this, final_time, dt)
        class(mtln_t) :: this
        real, intent(in) :: final_time
        integer :: n_time_steps
        real, intent(in), optional :: dt
        integer :: i

        if (present(dt) .and. dt /= 0) then
            this%dt = dt
            call this%updateBundlesTimeStep(dt)
        end if  

        n_time_steps = this%getTimeRange(final_time)
        ! call this%computeNWVoltageTerms()
        call this%updatePULTerms(n_time_steps)

        do i = 1, n_time_steps
            call this%step()
        end do

    end subroutine

    subroutine mtln_setBundle(this, name, bundle)
        class(mtln_t) :: this
        character(len=*), intent(in) :: name
        class(mtl_bundle_t), intent(in) :: bundle
        call this%bundles%set(key(name), value = bundle)
        if (bundle%dt < this%dt) then
            this%dt = bundle%dt
        end if

    end subroutine

    function mtln_getBundle(this, name, found) result(res)
        class(mtln_t) :: this
        type(mtl_bundle_t) :: res
        character(len=*), intent(in) :: name
        integer :: stat
        logical, intent(out), optional :: found
        class(*), allocatable :: d
  
        if (present(found)) found = .false.

        call this%bundles%get_raw(key(name), d, stat)
        if (stat /= 0) return
  
        select type(d)
         type is (mtl_bundle_t)
           res = d
           if (present(found)) found = .true.
        end select

    end function

    function mtln_findBundle(this, name, found) result(res)
        class(mtln_t) :: this
        class(*), pointer :: res
        class(fhash_key_t), allocatable :: name
        integer :: stat
        logical, intent(out), optional :: found
        class(*), allocatable :: d


        if (present(found)) found = .false.
        call this%bundles%get_raw_ptr(name, res, stat = stat)
        if (stat /= 0) return

        select type(res)
            type is(mtl_bundle_t)
            if (present(found)) found = .true.
        end select

    end function

end module mtln_solver_mod