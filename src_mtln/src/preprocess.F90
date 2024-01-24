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

    end type

    interface preprocess_t
        module procedure preprocessCtor
    end interface

contains

    function buildMTLArray(cable_array, materials) result(res)
        type(cable_array_t), intent(in) :: cable_array
        type(mtl_t), dimension(:), allocatable :: res
        type(fhash_tbl_t), value :: materials
        class(*), allocatable :: d
        integer ::stat, i
        allocate(res(0))
        do i = 1, size(cable_array%cables)
            call materials%get_raw(key(cable_array%cables%material_id), d, stat)
            if (stat /= 0) return
            select type(d)
                type is(wire_t)
                    res = [res, buildMTLFromWire(d)]
                    !build mtl
                type is(multiwire_t)
                    res = [res, buildMTLFromMultiwire(d)]
                    !build mtl
            end select
        end do  
        
    end function

    function buildMTLFromWire(wire) result(res)
        type(wire_t) :: wire
        type(mtl_t) :: res
        integer :: i
    end function 

    function buildMTLFromMultiwire(multiwire) result(res)
        type(multiwire_t) :: multiwire
        type(mtl_t) :: res
        integer :: i
    end function 

    function buildBundle(colinear_cables) result(bundle)
        type(cable_array_t), intent(in) :: colinear_cables
        type(cable_array_t) :: ordered_bundle_level
        type(bundle_t) :: bundle
        integer :: i, j

        logical, dimension(:), allocatable :: visited

        allocate(bundle%levels(0))
        allocate(visited(size(colinear_cables%cables)), source = .false.)

        do i = 1, size(colinear_cables%cables)
            if (associated(colinear_cables%cables(i)%parent_cable) .eqv. .false.) then 
                stop
            end if
        end do
        ordered_bundle_level%cables = [colinear_cables%cables(i)]
        visited(i) = .true.
        bundle%levels = [bundle%levels, ordered_bundle_level]
        do while (findNextLevel(ordered_bundle_level, colinear_cables%cables, visited) /= 0)
            bundle%levels = [bundle%levels, ordered_bundle_level]
        end do

        contains
            function findNextLevel(curr_level, cables, is_visited) result (next)
                type(cable_array_t), intent(inout) :: curr_level
                type(cable_t), dimension(:), intent(in) :: cables
                type(cable_t), target :: tgt
                logical, dimension(:), intent(inout) :: is_visited
                type(cable_array_t) :: nxt_level
                integer :: next
                do i = 1, size(curr_level%cables) 
                    tgt = curr_level%cables(i)
                    allocate(nxt_level%cables(0))
                    do j = 1, size(cables)
                        if (is_visited(i) .eqv. .false.) then 
                            if (associated(cables(j)%parent_cable, tgt)) then 
                                nxt_level%cables = [nxt_level%cables, cables(i)]
                                is_visited(i) = .true.
                            end if
    
                        end if
                    end do
                end do
                
                next = size(nxt_level%cables)
                curr_level = nxt_level
            end function

    end function

    ! cable with the same start and end relative coordinates belong to the same bundle
    function groupColinearCables(cables, elements) result(res)
        type(cable_t), dimension(:), allocatable, intent(in) :: cables
        type(cable_array_t) :: cable_array
        type(fhash_tbl_t), intent(in) :: elements
        type(fhash_tbl_t) :: map ! key: integer, dimension(2,3) | value: cable_array
        type(cable_array_t), dimension(:), allocatable :: res
        class(*), allocatable :: line, b, c_array
        
        integer :: i
        integer :: id, stat
        
        type(fhash_iter_t) :: iter
        class(fhash_key_t), allocatable :: k

        do i = 1, size(cables)
            id = cables(i)%element_ids(1)
            call elements%get_raw(key(id), line)
            select type(line) 
            type is(polyline_t)
                call map%check_key(key(line%coordinates), stat)
                if (stat /= 0) then !not found
                    cable_array%cables = [cables(i)]
                    call map%set(key(line%coordinates), value = cable_array)
                else  ! found
                    call map%get_raw(key(line%coordinates), b)
                    select type(b)
                    type is(cable_array_t)
                        b%cables = [b%cables, cables(i)]
                        call map%set(key(line%coordinates), value = b)
                    end select
                end if
            end select
        end do

        allocate(res(0))
        iter = fhash_iter_t(map)
        do while(iter%next(k,c_array))
            select type(c_array)
            type is(cable_array_t)
                res = [res,c_array]
            end select
        end do

    end function

    function preprocessCtor(parsed) result(res)
        type(parsed_t) :: parsed
        type(preprocess_t) :: res
        !ToDo
        type(cable_array_t), dimension(:), allocatable :: colinear_cables

        integer :: i, j
        type(bundle_t) :: bundle

        type(mtl_t), dimension(:), allocatable :: mtls
        type(mtl_array_t) :: mtl_levels
        type(mtl_bundle_t) :: mtl_bundle
        allocate(res%bundles(0))
        allocate(mtl_levels%levels(0))
        
        colinear_cables = groupColinearCables(parsed%cables, parsed%elements)
            
        do i = 1, size(colinear_cables)
            bundle = buildBundle(colinear_cables(i))
            do j = 1, size(bundle%levels)
                mtl_levels%levels = [mtl_levels%levels, buildMTLArray(bundle%levels(i),parsed%materials)]
            end do
            mtl_bundle = mtldCtor(mtl_levels)
            res%bundles = [res%bundles,mtl_bundle]
        end do


    end function


    
end module