module preprocess_mod

    use mtln_types_mod
    use mtl_bundle_mod
    use mtl_mod

    ! use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    use fhash, only: fhash_tbl_t
    use fhash_key_coordinate_pair, only: fhash_key_coordinate_pair_t, key=>fhash_key
    
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

    type, public :: cable_array_t
        type(cable_t), dimension(:), allocatable :: cables
    end type

contains

    ! elemental type(mtl_t) function buildLines(cable, materials)
    !     type(cable_t), intent(in) :: cable
    !     type(fhash_tbl_t), value :: materials
    !     class(*), allocatable :: d
    !     integer ::stat
    !     !Reference to impure function 'key_from_int32' at (1) within a PURE procedure
    !     call materials%get_raw(key(cable%material_id), d, stat)
    !     if (stat /= 0) return
    !     select type(d)
    !         type is(wire_t)
    !             !build mtl
    !         type is(multiwire_t)
    !             !build mtl
    !     end select
        
    ! end function

    ! function buildLines(cables) result(res)
    !     type(cable_t), dimension(:), allocatable :: cables
    !     type(mtl_t), dimension(:), allocatable :: res
    !     integer :: i


    ! end function 

    function orderBundle(bundle) result(ordered_bundle)
        type(cable_array_t), intent(in) :: bundle
        type(cable_array_t) :: ordered_bundle_level
        type(cable_array_t), pointer :: next_level
        type(cable_t), pointer :: cable_ptr
        type(fhash_tbl_t) :: ordered_bundle ! int : cable_array
        class(*), allocatable :: b
        integer :: i, j, n

        logical, dimension(:), allocatable :: visited
        allocate(visited(size(bundle%cables)), source = .false.)

        do i = 1, size(bundle%cables)
            if (associated(bundle%cables(i)%parent_cable) .eqv. .false.) then 
                stop
            end if
        end do
        ordered_bundle_level%cables = [bundle%cables(i)]
        visited(i) = .true.
        n = 0
        call ordered_bundle%set(key(0), value = ordered_bundle_level)
       
        do while (findNextLevel(ordered_bundle_level, bundle%cables, visited) /= 0)
            n = n + 1
            call ordered_bundle%set(key(n), value = ordered_bundle_level)
        end do

        ! ordered_bundle should contain one integer key for each level. The values are 

        ! do i = 1, size(bundle%cables)
        !     n = 0
        !     cable_ptr => bundle%cables(i)%parent_cable
        !     do while (associated(cable_ptr) .eqv. .true.)
        !         n = n + 1
        !         cable_ptr => cable_ptr%parent_cable
        !     end do
        !     block
        !         type(cable_array_t) :: ordered_bundle_level
        !         integer :: stat = 0
        !         call ordered_bundle%check_key(key(n), stat)
        !         if (stat /= 0) then !not found
        !             ! ordered_bundle_level%cables = [ordered_bundle_level%cables(i)]
        !             call ordered_bundle%set(key(n), value = ordered_bundle_level)
        !         else  ! found
        !             call ordered_bundle%get_raw(key(n), b)
        !             select type(b)
        !             type is(cable_array_t)
        !                 b%cables = [b%cables, bundle%cables(i)]
        !                 call ordered_bundle%set(key(n), value = b)
        !             end select
        !         end if
        !     end block

        ! end do

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
                                next_level%cables = [next_level%cables, cables(i)]
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
    function groupCablesIntoBundles(cables, elements) result(res)
        type(cable_t), dimension(:), allocatable, intent(in) :: cables
        type(cable_array_t) :: bundle
        type(fhash_tbl_t), intent(in) :: elements
        type(fhash_tbl_t) :: res ! key: integer, dimension(2,3) | value: bundle
        class(*), allocatable :: line, b
        
        integer :: i
        integer :: id, stat

        do i = 1, size(cables)
            id = cables(i)%element_ids(1)
            call elements%get_raw(key(id), line)
            select type(line) 
            type is(polyline_t)
                call res%check_key(key(line%coordinates), stat)
                if (stat /= 0) then !not found
                    bundle%cables = [cables(i)]
                    call res%set(key(line%coordinates), value = bundle)
                else  ! found
                    call res%get_raw(key(line%coordinates), b)
                    select type(b)
                    type is(cable_array_t)
                        b%cables = [b%cables, cables(i)]
                        call res%set(key(line%coordinates), value = b)
                    end select
                end if
            end select
        end do

        ! call bundles%set(key(name), value = bundle)
    end function

    function preprocessCtor(parsed) result(res)
        type(parsed_t) :: parsed
        type(preprocess_t) :: res
        !ToDo
        type(mtl_t), dimension(:), allocatable :: mtls
        ! mtls = buildLines(parsed%cables, parsed%materials)
            

    end function

end module