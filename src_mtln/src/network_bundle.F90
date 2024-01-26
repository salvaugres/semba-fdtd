module network_bundle_mod

    use fhash, only: fhash_key_t
    use mtl_bundle_mod
    use network_mod
    use types_mod, only: network_iter_t
    implicit none

    type, public :: network_bundle_t
        type(fhash_tbl_t) :: levels ! dict{level_number : network_t}
    contains
        procedure :: updateIndexNumbers
        ! procedure :: updateSources
        procedure :: advanceVoltage => network_bundle_advanceVoltage
        procedure :: updateBundlesVoltages => network_bundle_updateBundlesVoltages
        procedure :: updateCurrents => network_bundle_updateCurrents
        ! procedure :: computeNWVoltageTerms => network_bundle_computeNWVoltageTerms

    end type network_bundle_t

    interface network_bundle_t
        module procedure network_bundleCtor
    end interface

contains

    function network_bundleCtor(levels) result(res)
        type(network_bundle_t) :: res
        type(fhash_tbl_t) :: levels
    end function 

    subroutine updateIndexNumbers(this, bundles)
        class(network_bundle_t) :: this
        class(fhash_tbl_t), intent(in) :: bundles

        type(network_iter_t) :: iter
        class(fhash_key_t), allocatable :: name
        class(network_t), pointer :: network
        iter = network_iter_t(this%levels)
        do while(iter%findNext(name,network))
        ! TODO
        !     for node in nw.connections.values():
        !         bundle = bundles[node["bundle_name"]]
        !         conductors_above = sum(bundle.conductors_in_level[0:level])
        !         node["line_index"] += conductors_above
        enddo
    end subroutine 

    ! subroutine updateSources(this, time, dt)
    !     class(network_bundle_t) :: this
    !     real, intent(in) :: time, dt
    !     type(network_iter_t) :: iter
    !     class(fhash_key_t), allocatable :: name
    !     class(network_t), pointer :: network

    !     iter = network_iter_t(this%levels)
    !     do while(iter%findNext(name,network))
    !         call network%updateSources(time, dt)
    !     enddo
    ! end subroutine

    subroutine network_bundle_advanceVoltage(this, dt)
        class(network_bundle_t) :: this
        real, intent(in) :: dt
        type(network_iter_t) :: iter
        class(fhash_key_t), allocatable :: name
        class(network_t), pointer :: network

        iter = network_iter_t(this%levels)
        do while(iter%findNext(name,network))
            call network%advanceVoltage(dt)
        enddo
    end subroutine

    subroutine network_bundle_updateBundlesVoltages(this, bundles)
        class(network_bundle_t) :: this
        class(fhash_tbl_t), intent(in) :: bundles
        type(network_iter_t) :: iter
        class(fhash_key_t), allocatable :: name
        class(network_t), pointer :: network

        iter = network_iter_t(this%levels)
        do while(iter%findNext(name,network))
            call network%updateBundlesVoltages(bundles)
        enddo
    end subroutine 

    subroutine network_bundle_updateCurrents(this, bundles)
        class(network_bundle_t) :: this
        class(fhash_tbl_t), intent(in) :: bundles
        type(network_iter_t) :: iter
        class(fhash_key_t), allocatable :: name
        class(network_t), pointer :: network

        iter = network_iter_t(this%levels)
        do while(iter%findNext(name,network))
            call network%updateCurrents(bundles)
        enddo
    end subroutine 

    ! subroutine network_bundle_computeNWVoltageTerms(this, dt)
    !     class(network_bundle_t) :: this
    !     real, intent(in) :: dt
    !     type(network_iter_t) :: iter
    !     class(fhash_key_t), allocatable :: name
    !     class(network_t), pointer :: network

    !     iter = network_iter_t(this%levels)
    !     do while(iter%findNext(name,network))
    !         call network%computeNWVoltageTerms(dt)
    !     enddo

    ! end subroutine 



end module network_bundle_mod