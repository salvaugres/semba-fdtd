integer function test_fhash() bind(C) result(error_cnt)

    use mtl_bundle_mod
    use fhash, only: fhash_tbl_t, key=>fhash_key
    ! , key=>fhash_key, fhash_iter_t, fhash_key_t

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

integer function test_fhash_ptr() bind(C) result(error_cnt)

    use mtl_bundle_mod
    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    use types_mod, only: bundle_iter_t

    implicit none

    type(fhash_tbl_t) :: tbl
    type(bundle_iter_t) :: iter
    ! type(fhash_iter_t) :: iter
    type(mtl_bundle_t) :: bundle1, bundle2
    class(fhash_key_t), allocatable :: name
    class(*), allocatable :: bundle
    class(mtl_bundle_t), pointer :: bundle_ptr
    integer :: stat, n_cond
    error_cnt = 0

    bundle1%name = "bundle1"
    bundle1%number_of_conductors = 5
    bundle2%name = "bundle2"
    bundle2%number_of_conductors = 10


    call tbl%set(key(bundle1%name), value = bundle1)
    call tbl%set(key(bundle2%name), value = bundle2)

    iter = bundle_iter_t(tbl)
    
    do while(iter%findNext(name,bundle_ptr))
        bundle_ptr%number_of_conductors = bundle_ptr%number_of_conductors*5

    end do

    call tbl%get_raw(key(bundle1%name), bundle,stat)
    select type(bundle)
    type is (mtl_bundle_t)
        if (.not.(bundle%number_of_conductors == 25)) then
            error_cnt = error_cnt + 1
        endif
    end select

    call tbl%get_raw(key(bundle2%name), bundle,stat)
    select type(bundle)
    type is (mtl_bundle_t)
    if (.not.(bundle%number_of_conductors == 50)) then
        error_cnt = error_cnt + 1
    endif
end select


end function

