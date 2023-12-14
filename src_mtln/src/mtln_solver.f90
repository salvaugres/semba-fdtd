module mtln_solver_mod 

    ! use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_iter_t, fhash_key_t
    use mtl_bundle_mod
    use network_bundle_mod
    implicit none

    type, public :: mtln_t
        real :: time, dt
        ! type(fhash_tbl_t) :: bundles
        type(mtl_bundle_t), allocatable, dimension(:) :: bundles !lista de MLTD
        type(network_bundle_t), allocatable, dimension(:) :: networks !lista de NetworkD, no de network
        character(len=:), allocatable, dimension(:) :: names
    contains

        ! procedure :: setBundle => mtln_setBundle
        ! procedure :: getBundle => mtln_getBundle
        procedure :: updateBundlesTimeStep
        procedure :: updatePULTerms
        procedure :: addNetwork
        ! procedure :: addBundle
        procedure :: getTimeRange
        procedure :: computeNWVoltageTerms
        procedure :: runUntil
        procedure :: updateProbes
        procedure :: updateNWCurrent
        procedure :: advanceNWVoltage
        procedure :: advanceBundlesVoltage
        procedure :: advanceBundlesCurrent
        procedure :: advanceTime
        procedure :: step


    end type mtln_t


    interface mtln_t
        module procedure mtlnCtor
    end interface

contains

    function mtlnCtor(bundles, networks) result(res)
        type(mtln_t) :: res
        class(mtl_bundle_t), dimension(:) :: bundles
        class(network_bundle_t), dimension(:) :: networks
        integer :: i

        res%dt = 1e10
        res%time  = 0.0
        allocate(res%networks(0))
        do i = 1, size(bundles)
            ! call res%setBundle(bundles(i)%name, bundles(i))
            ! call res%addBundle(bundles(i))
        end do
        do i = 1, size(networks)
            call res%addNetwork(networks(i))
        end do

    end function

    subroutine step(this)
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
        !TODO
    end subroutine

    subroutine advanceNWVoltage(this)
        class(mtln_t) :: this
        integer :: i
        do i = 1, size(this%networks)
            call this%networks(i)%updateSources(this%time, this%dt)
            call this%networks(i)%advanceVoltage(this%dt)
            call this%networks(i)%updateVoltages(this%bundles)
        end do
    end subroutine

    subroutine advanceBundlesCurrent(this)
        class(mtln_t) :: this
        !TODO
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
        ! type(fhash_iter_t) :: iter
        ! class(fhash_key_t), allocatable :: name
        ! class(*), allocatable :: bundle
        ! integer :: i
        ! iter = fhash_iter_t(this%bundles)
        ! do while(iter%next(name,bundle))
        !     select type(bundle)
        !     type is (mtl_bundles_t)
                
        !         do i = 1, size(bundle%probes)
        !             ! bundle%probes(i)%update(this%time, bundle%v, bundle%i)
        !         end do
        !         ! call this%setBundle(name%to_string(), bundle)                      

        !     end select
        ! end do

    end subroutine


    subroutine addNetwork(this, network)
        class(mtln_t) :: this
        class(network_bundle_t) :: network
        !call network%updateIndexNumbers(this%bundles)
        this%networks = [this%networks, network]
    end subroutine


    function getTimeRange(this, final_time) result(res)
        class(mtln_t) :: this
        real, intent(in) :: final_time
        integer :: res
        integer :: i
        res =  floor(final_time / this%dt)
    end function

    subroutine computeNWVoltageTerms(this)
        class(mtln_t) :: this
        integer :: i
        do i = 1, size(this%networks)
            call this%networks(i)%computeVoltageTerms
        end do
    end subroutine

    subroutine updateBundlesTimeStep(this, dt)
        class(mtln_t) :: this
        ! type(fhash_iter_t) :: iter
        ! class(fhash_key_t), allocatable :: name
        ! class(*), allocatable :: bundle
        real :: dt

        ! iter = fhash_iter_t(this%bundles)
        ! do while(iter%next(name,bundle))
        !     select type(bundle)
        !     type is (mtl_bundles_t)
        !         bundle%dt = dt
        !         ! call this%setBundle(name%to_string(), bundle)                      
        !    end select
        ! end do

    end subroutine

    subroutine updatePULTerms(this)
        class(mtln_t) :: this
        ! type(fhash_iter_t) :: iter
        ! class(fhash_key_t), allocatable :: name
        ! class(*), allocatable :: bundle

        ! iter = fhash_iter_t(this%bundles)
        ! do while(iter%next(name,bundle))
        !     select type(bundle)
        !     type is (mtl_bundles_t)
        !         call bundle%updateLRTerms()
        !         call bundle%updateCGTerms()
        !         ! for p in bundle.probes:
        !         !     p.resize_frames(len(t), bundle.number_of_conductors)
        !         ! call this%setBundle(name%to_string(), bundle)                      
        !     end select
        ! end do
   
    end subroutine

    subroutine runUntil(this, final_time, dt)
        class(mtln_t) :: this
        real, intent(in) :: final_time
        real, intent(in), optional :: dt
        integer :: i

        if (present(dt) .and. dt /= 0) then
            this%dt = dt
            call this%updateBundlesTimeStep(dt)
        end if  

        call this%computeNWVoltageTerms()
        call this%updatePULTerms()

        do i = 1, this%getTimeRange(final_time)
            call this%step()
        end do

    end subroutine

    ! subroutine mtln_setBundle(this, name, bundle)
    !     class(mtln_t) :: this
    !     character(len=*), intent(in) :: name
    !     class(mtl_bundles_t), intent(in) :: bundle
    !     call this%bundles%set(key(name), value = bundle)
    !     if (bundle%dt < this%dt) then
    !         this%dt = bundle%dt
    !     end if

    ! end subroutine

    ! function mtln_getBundle(this, name, found) result(res)
    !     class(mtln_t) :: this
    !     type(mtl_bundles_t) :: res
    !     character(len=*), intent(in) :: name
    !     integer :: stat
    !     logical, intent(out), optional :: found
    !     class(*), allocatable :: d
  
    !     if (present(found)) found = .false.

    !     call this%bundles%get_raw(key(name), d, stat)
    !     if (stat /= 0) return
  
    !     select type(d)
    !      type is (mtl_bundles_t)
    !        res = d
    !        if (present(found)) found = .true.
    !     end select

    ! end function

    ! subroutine (this)
    !     class(mtln_t) :: this
    !     !TODO
    ! end subroutine 

end module mtln_solver_mod