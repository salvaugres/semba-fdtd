module preprocess_mod

    use mtln_types_mod
    use mtl_bundle_mod
    use mtl_mod

    ! use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    use fhash, only: fhash_tbl_t
    use fhash_key_coordinate_pair, only: fhash_key_coordinate_pair_t, key=>fhash_key
    use fhash_data_container, only: fhash_container_t
    
    use network_bundle_mod
    implicit none
    !preprocess: gets parser info, generates bundles and networks


    type, public :: preprocess_t
        class(mtl_bundle_t), dimension(:), allocatable :: bundles ! dict{name:bundle} of MLTD
        type(network_bundle_t), dimension(:), allocatable :: networks !lista de NetworkD, no de network

        real, private :: priv_real
    
    contains
        ! procedure :: buildLines
        ! procedure :: groupCollinearCables

    end type

    interface preprocess_t
        module procedure preprocess
    end interface

contains

    ! function buildMTLArray(cable_array, parsed) result(res)
    !     type(cable_array_t), intent(in) :: cable_array
    !     type(mtl_t), dimension(:), allocatable :: res
    !     type(parsed_t) :: parsed
    !     class(*), allocatable :: d
    !     integer ::stat, i
    !     allocate(res(0))
    !     do i = 1, size(cable_array%cables)
    !         call parsed%materials%get_raw(key(cable_array%cables(i)%material_id), d, stat)
    !         if (stat /= 0) return
    !         select type(d)
    !             type is(wire_t)
    !                 res = [res, buildMTLFromWire(d, cable_array%cables(i), parsed%mesh)]
    !                 !build mtl
    !             type is(multiwire_t)
    !                 res = [res, buildMTLFromMultiwire(d, parsed)]
    !                 !build mtl
    !         end select
    !     end do  
        
    ! end function

    ! function buildMTLFromWire(wire, cable, mesh) result(res)
    !     type(wire_t), intent(in) :: wire
    !     type(cable_t), intent(in) :: cable
    !     type(mesh_t), intent(in) :: mesh
    !     type(mtl_t) :: res
    !     integer :: i, stat
    !     class(*), allocatable :: poly
    !     real, dimension(3) :: init, end

    !     ! i = cable%element_ids(1)
    !     ! call mesh%elements%get_raw(key(i), poly, stat)
    !     ! if (stat /= 0) then 
    !     !     select type(poly)
    !     !     type(polyline_t)
    !     !         init = poly%coordinates(1)*mesh%grid%steps_in_direction(1)%steps
    !     !     end select
    !     ! else
    !     !     !error
    !     ! end if
    !     ! res = mtl_t(lpul = eye(1)*wire%ref_inductance_per_meter, &
    !     !             cpul = eye(1)*wire%ref_capacitance_per_meter, &
    !     !             rpul = eye(1)*wire%resistance_per_meter, & 
    !     !             gpul = eye(1)*0, &
    !     !             node_positions = ,&
    !     !             divisions =  ,&
    !     !             name = cable%name)

    ! end function 

    ! function buildMTLFromMultiwire(multiwire, parsed) result(res)
    !     type(multiwire_t), intent(in) :: multiwire
    !     type(parsed_t), intent(in) :: parsed
    !     type(mtl_t) :: res
    !     integer :: i
    ! end function 


    ! function preprocessCtor(parsed) result(res)
    !     type(parsed_t) :: parsed
    !     type(preprocess_t) :: res
    !     !ToDo
    !     type(cable_array_t), dimension(:), allocatable :: colinear_cables

    !     integer :: i, j
    !     type(bundle_t) :: bundle

    !     type(mtl_t), dimension(:), allocatable :: mtls
    !     type(mtl_array_t) :: mtl_levels
    !     type(mtl_bundle_t) :: mtl_bundle
    !     allocate(res%bundles(0))
    !     allocate(mtl_levels%levels(0))
        
    !     colinear_cables = groupColinearCables(parsed%cables, parsed%mesh%elements)
            
    !     do i = 1, size(colinear_cables)
    !         bundle = buildBundle(colinear_cables(i))
    !         do j = 1, size(bundle%levels)
    !             mtl_levels%levels = [mtl_levels%levels, buildMTLArray(bundle%levels(i),parsed)]
    !         end do
    !         mtl_bundle = mtldCtor(mtl_levels)
    !         res%bundles = [res%bundles,mtl_bundle]
    !     end do


    ! end function

    function buildBundleFromParent(parent, cables) result(res)
        type(cable_t), intent(in) :: parent
        type(cable_t), dimension(:), intent(in) :: cables
        type(cable_array_t) :: level
        type(cable_bundle_t) :: res

        allocate(res%levels(1))
        level%cables = [parent]
        res%levels(1) = level

        do while (findNextLevel(level) /= 0)
            res%levels = [res%levels, level]
        end do

        contains
            integer function findNextLevel(curr_level)
                type(cable_array_t), intent(inout) :: curr_level
                type(cable_t), target :: tgt
                type(cable_array_t) :: next_level
                integer :: i,j
                allocate(next_level%cables(0))
                do i = 1, size(curr_level%cables) 
                    tgt = curr_level%cables(i)
                    do j = 1, size(cables)
                        if (associated(cables(j)%parent_cable, tgt)) then 
                            next_level%cables = [next_level%cables, cables(j)]
                        end if
                    end do
                end do
                curr_level = next_level
                findNextLevel = size(curr_level%cables)
            end function

    end function

    function findParentCables(cables) result(res)
        type(cable_t), dimension(:), intent(in) :: cables
        type(cable_t), dimension(:), allocatable :: res
        integer :: i
        allocate(res(0))
        do i = 1, size(cables)
            if (associated(cables(i)%parent_cable) .eqv. .false.) then 
                res = [res, cables(i)]
            end if
        end do
    end function


    function buildBundlesFromCables(cables) result(bundles)
        type(cable_t), dimension(:), intent(in) :: cables
        type(cable_bundle_t), dimension(:), allocatable :: bundles
        type(cable_t), dimension(:), allocatable :: parents
        integer :: i

        allocate(bundles(0))
        parents = findParentCables(cables)
        do i = 1, size(parents)
            bundles = [bundles, buildBundleFromParent(parents(i), cables)]
        end do

    end function

    function preprocess(parsed) result(res)
        type(parsed_t), intent(in):: parsed
        type(preprocess_t) :: res

        type(cable_bundle_t), dimension(:), allocatable :: bundles ! dimension eq to number of bundles

        bundles = buildBundlesFromCables(parsed%cables)


    end function
    
end module