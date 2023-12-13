module networks_bundles_mod

    type, public :: network_bundles_t

    contains
        ! procedure :: updateIndexNumbers
        ! procedure :: updateSources
        ! procedure :: advanceVoltage
        ! procedure :: updateVoltages
        ! procedure :: updateCurrents
        ! procedure :: computeNWVTerms


    end type network_bundles_t

    interface network_bundles_t
        module procedure networkCtor
    end interface network_bundles_t

contains

    function networkCtor() result(res)
        type(network_bundles_t) :: res
    end function networkCtor

    subroutine updateIndexNumbers(this)
        type(network_bundles_t) :: res
        !TODO
    end subroutine updateIndexNumbers

    subroutine updateSources(this)
        type(network_bundles_t) :: res
        !TODO
    end subroutine updateSources

    subroutine advanceVoltage(this)
        type(network_bundles_t) :: res
        !TODO
    end subroutine advanceVoltage

    subroutine updateVoltages(this)
        type(network_bundles_t) :: res
        !TODO
    end subroutine updateVoltages

    subroutine updateCurrents(this)
        type(network_bundles_t) :: res
        !TODO
    end subroutine updateCurrents

    subroutine computeNWVoltageTerms(this)
        type(network_bundles_t) :: res
        !TODO
    end subroutine computeNWVoltageTerms


end module networks_bundles_mod