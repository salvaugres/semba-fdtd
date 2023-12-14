module network_bundle_mod

    use mtl_bundle_mod

    implicit none
    type, public :: network_bundle_t

    contains
        procedure :: updateIndexNumbers
        procedure :: updateSources
        procedure :: advanceVoltage
        procedure :: updateVoltages
        procedure :: updateCurrents
        procedure :: computeVoltageTerms


    end type network_bundle_t

    interface network_bundle_t
        module procedure networkCtor
    end interface

contains

    function networkCtor() result(res)
        type(network_bundle_t) :: res
    end function 

    subroutine updateIndexNumbers(this)
        class(network_bundle_t) :: this
        !TODO
    end subroutine 

    subroutine updateSources(this, time, dt)
        class(network_bundle_t) :: this
        real, intent(in) :: time, dt
        !TODO
    end subroutine

    subroutine advanceVoltage(this, dt)
        class(network_bundle_t) :: this
        real, intent(in) :: dt
        !TODO
    end subroutine

    subroutine updateVoltages(this, bundles)
        class(network_bundle_t) :: this
        class(mtl_bundle_t), dimension(:), intent(in) :: bundles
        !TODO
    end subroutine 

    subroutine updateCurrents(this, bundles)
        class(network_bundle_t) :: this
        class(mtl_bundle_t), dimension(:), intent(in) :: bundles
        !TODO
    end subroutine 

    subroutine computeVoltageTerms(this)
        class(network_bundle_t) :: this
        !TODO
    end subroutine 


end module network_bundle_mod