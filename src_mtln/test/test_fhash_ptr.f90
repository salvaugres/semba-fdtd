integer function test_fhash_ptr() result(error_cnt)

    use mtl_bundle_mod
    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_iter_t, fhash_key_t

    implicit none

    type(fhash_tbl_t) :: tbl
    type(fhash_iter_t) :: iter
    type(mtl_bundle_t) :: bundle1, bundle2
    class(fhash_key_t), allocatable :: name
    class(*), allocatable :: bundle
    class(*), pointer :: ptr 
    integer :: stat, n_cond
    error_cnt = 0

    bundle1%name = 'bundle1'
    bundle1%number_of_conductors = 5
    bundle2%name = 'bundle2'
    bundle2%number_of_conductors = 10


    call tbl%set(key(bundle1%name), value = bundle1)
    call tbl%set(key(bundle2%name), value = bundle2)

    iter = fhash_iter_t(tbl)
    
    do while(iter%next(name,bundle))
        ! select type(bundle)
        ! type is(mtl_bundle_t)
        !     ptr=>bundle
        ! end select
        call tbl%get_raw_ptr(name, ptr, stat = stat)
        select type(ptr)
        type is(mtl_bundle_t)
            ptr%number_of_conductors = ptr%number_of_conductors*5
        end select

    end do

    call tbl%get_raw(key(bundle1%name), bundle,stat)
    select type(bundle)
    type is (mtl_bundle_t)
        write(*,*) 'number of conductors in 1) : ', bundle%number_of_conductors
    end select

    call tbl%get_raw(key(bundle2%name), bundle,stat)
    select type(bundle)
    type is (mtl_bundle_t)
        write(*,*) 'number of conductors in 2) : ', bundle%number_of_conductors
    end select


end function



