module mtln_mod 

    use domain_mod
    use networks_mod
    implicit none

    type, public :: mtln_t
        real :: time, dt
        ! diccionarios(hash?) bundles, du
        type(network_t), allocatable, dimension(:) :: networks
    contains
        procedure :: addBundle
        procedure :: addNetwork
        procedure :: addNetworks
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
    end interface mtln_t

contains

    function mtlnCtor() result(res)
        type(mtln_t) :: res
    end function mtlnCtor

    subroutine step(this)
        class(mtln_t) :: this

        call this%advanceBundlesVoltage()
        call this%advanceNWVoltage()
        call this%advanceBundlesCurrent()
        call this%updateNWCurrent()

        call this%advanceTime()
        call this%updateProbes()

    end subroutine step

    subroutine advanceTime(this)
        class(mtln_t) :: this
        this%time = this%time + this%dt
    end subroutine advanceTime

    subroutine updateProbes(this)
        class(mtln_t) :: this
        !TODO
    end subroutine updateProbes

    subroutine advanceBundlesVoltage(this)
        class(mtln_t) :: this
        !TODO
    end subroutine advanceBundlesVoltage

    subroutine advanceNWVoltage(this)
        class(mtln_t) :: this
        !TODO
    end subroutine advanceNWVoltage

    subroutine advanceBundlesCurrent(this)
        class(mtln_t) :: this
        !TODO
    end subroutine advanceBundlesCurrent

    subroutine updateNWCurrent(this)
        class(mtln_t) :: this
        !TODO
    end subroutine updateNWCurrent

    subroutine addBundle(this, bundle)
        class(mtln_t) :: this
        class(domain_t) :: bundle
        !TODO
        if (bundle%dt < this%dt) then
            this%dt = bundle%dt
        end if
    end subroutine addBundle

    subroutine addNetwork(this)
        class(mtln_t) :: this
        !TODO
    end subroutine addNetwork

    subroutine addNetworks(this)
        class(mtln_t) :: this
        !TODO
    end subroutine addNetworks

    subroutine getTimeRange(this)
        class(mtln_t) :: this
        !TODO
    end subroutine getTimeRange

    subroutine computeNWVoltageTerms(this)
        class(mtln_t) :: this
        !TODO
    end subroutine computeNWVoltageTerms

    subroutine runUntil(this)
        class(mtln_t) :: this
        !TODO
    end subroutine runUntil

    ! subroutine (this)
    !     class(mtln_t) :: this
    !     !TODO
    ! end subroutine 

end module mtln_mod