integer function test_fhash() bind(C) result(error_cnt)

    use mtl_bundle_mod
    use fhash, only: fhash_tbl_t, key=>fhash_key
    

    implicit none

    type(fhash_tbl_t) :: tbl
    type(mtl_bundle_t) :: bundle
    class(*), allocatable :: bundle_from_hash_1, bundle_from_hash_2
    class(*), pointer :: ptr 
    integer :: stat, n_cond
    error_cnt = 0

    bundle%name = "bundle1"
    bundle%number_of_conductors = 5

    call tbl%set(key(bundle%name), value = bundle)

    call tbl%get_raw(key(bundle%name), bundle_from_hash_1,stat)
    
    select type(bundle_from_hash_1)
    type is (mtl_bundle_t)
        bundle_from_hash_1%number_of_conductors = 100
        call tbl%set(key(bundle_from_hash_1%name), value = bundle_from_hash_1)
    end select

    call tbl%get_raw(key(bundle%name), bundle_from_hash_2,stat)
    select type(bundle_from_hash_2)
    type is (mtl_bundle_t)
        n_cond = bundle_from_hash_2%number_of_conductors
    end select
    
    write(*,*) 'number of conductors ', n_cond
    if (n_cond /= 100) then
        error_cnt = error_cnt +1
    end if

end function


integer function test_fhash_arrays() bind(C) result(error_cnt)

    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    implicit none

    type pul_t 
        real, dimension(:,:), allocatable :: a
        real, dimension(:,:), allocatable :: b
    end type

    type(fhash_tbl_t) :: tbl, tbl_cables
    type(pul_t), target :: p1, p2
    type(pul_t), pointer :: p1_ptr, p2_ptr

    error_cnt = 0
    !allocate 1x1 matrices in p1
    allocate(p1%a(1,1), source = 1.0)
    allocate(p1%b(1,1), source = 1.0)
    
    !allocate 2x2 matrices in p2
    allocate(p2%a(2,2), source = 2.0)
    allocate(p2%b(2,2), source = 2.0)

    ! add puls to table
    call tbl%set(key(1), p1, pointer=.true.)
    call tbl%set(key(2), p2, pointer=.true.)

    p1_ptr => getPtrFromTable(1)
    p2_ptr => getPtrFromTable(2)

    if (.not. associated(p1_ptr, p1)) then 
        error_cnt = error_cnt + 1
    end if
    if (.not. associated(p2_ptr, p2)) then 
        error_cnt = error_cnt + 2
    end if

    contains 
        function getPtrFromTable(id) result(res)
            integer, intent(in) :: id
            integer :: mStat
            class(*), pointer :: d
            type(pul_t), pointer :: res
            
            nullify(res)
            call tbl%check_key(key(id), mStat)
            if (mStat /= 0) then
                return
            end if

            call tbl%get_raw_ptr(key(id), d, mStat)
            if (mStat /= 0) then
                return
            end if
            select type(d)
                type is (pul_t)
                    res => d
            end select
        end function


end function

integer function test_fhash_cables() bind(C) result(error_cnt)

    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    use mtln_types_mod, only: cable_t
    implicit none


    type(fhash_tbl_t) :: tbl
    type(cable_t), target :: c1, c2
    type(cable_t), pointer :: c1_ptr, c2_ptr

    error_cnt = 0
    !allocate 1x1 matrices in p1
    allocate(c1%resistance_per_meter(1,1), source = 1.0)
    allocate(c1%inductance_per_meter(1,1), source = 1.0)
    allocate(c1%capacitance_per_meter(1,1), source = 1.0)
    allocate(c1%conductance_per_meter(1,1), source = 1.0)
    
    !allocate 2x2 matrices in p2
    allocate(c2%resistance_per_meter(2,2), source = 2.0)
    allocate(c2%inductance_per_meter(2,2), source = 2.0)
    allocate(c2%capacitance_per_meter(2,2), source = 2.0)
    allocate(c2%conductance_per_meter(2,2), source = 2.0)

    ! add puls to table
    call tbl%set(key(1), c1, pointer=.true.)
    call tbl%set(key(2), c2, pointer=.true.)

    c1_ptr => getPtrFromTable(1)
    c2_ptr => getPtrFromTable(2)

    if (.not. associated(c1_ptr, c1)) then 
        error_cnt = error_cnt + 1
    end if
    if (.not. associated(c2_ptr, c2)) then 
        error_cnt = error_cnt + 2
    end if

    contains 
        function getPtrFromTable(id) result(res)
            integer, intent(in) :: id
            integer :: mStat
            class(*), pointer :: d
            type(cable_t), pointer :: res
            
            nullify(res)
            call tbl%check_key(key(id), mStat)
            if (mStat /= 0) then
                return
            end if

            call tbl%get_raw_ptr(key(id), d, mStat)
            if (mStat /= 0) then
                return
            end if
            select type(d)
                type is (cable_t)
                    res => d
            end select
        end function


end function

