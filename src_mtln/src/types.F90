module types_mod

    use fhash_tbl_iter
    use fhash_tbl, only: fhash_tbl_t
    use fhash_key_base, only: fhash_key_t
    use fhash_data_container, only: fhash_container_t
    use fhash_sll

    use mtl_bundle_mod
    use network_mod
    implicit none

    type, extends(fhash_iter_t) :: bundle_iter_t
    contains
        procedure :: findNext => bundle_iter_next_ptr
    end type 
 
    interface bundle_iter_t
        module procedure :: bundle_iter_init
    end interface bundle_iter_t

 
    type, extends(fhash_iter_t) :: network_iter_t
    contains
        procedure :: findNext => network_iter_next_ptr
    end type 
    
    interface network_iter_t
        module procedure :: network_iter_init
    end interface network_iter_t



 contains
 
    function bundle_iter_init(tbl) result(iter)
        type(fhash_tbl_t), intent(in), target :: tbl
        type(bundle_iter_t) :: iter

        iter%fhash_iter_t%tbl => tbl

    end function bundle_iter_init

    function bundle_iter_next_ptr(iter,key,ptr) result(found)
        class(bundle_iter_t), intent(inout) :: iter
        class(fhash_key_t), intent(out), allocatable :: key
        class(*), pointer :: data
        class(mtl_bundle_t), pointer, intent(out) :: ptr
        logical :: found
        
        type(fhash_container_t), pointer :: data_container
        
        found = .false.

        if (.not.associated(iter%tbl)) return

        do while (.not.found)
        if (iter%bucket > size(iter%tbl%buckets)) return
        if (.not.allocated(iter%tbl%buckets(iter%bucket)%key)) then
            iter%bucket = iter%bucket + 1
            cycle
        end if
        call sll_get_at(iter%tbl%buckets(iter%bucket),iter%depth,key,data_container,found)
        if (iter%depth > node_depth(iter%tbl%buckets(iter%bucket))) then
            iter%bucket = iter%bucket + 1
            iter%depth = 1
        else
            iter%depth = iter%depth + 1
        end if
        end do

        if (found) then
            call data_container%get_ptr(raw=data)
            select type(data)
                type is(mtl_bundle_t)
                ptr=>data
            end select

        end if

    end function 

    function network_iter_init(tbl) result(iter)
        type(fhash_tbl_t), intent(in), target :: tbl
        type(network_iter_t) :: iter

        iter%fhash_iter_t%tbl => tbl

    end function network_iter_init

    function network_iter_next_ptr(iter,key,ptr) result(found)
        class(network_iter_t), intent(inout) :: iter
        class(fhash_key_t), intent(out), allocatable :: key
        class(*), pointer :: data
        class(network_t), pointer, intent(out) :: ptr
        logical :: found
        
        type(fhash_container_t), pointer :: data_container
        
        found = .false.

        if (.not.associated(iter%tbl)) return

        do while (.not.found)
        if (iter%bucket > size(iter%tbl%buckets)) return
        if (.not.allocated(iter%tbl%buckets(iter%bucket)%key)) then
            iter%bucket = iter%bucket + 1
            cycle
        end if
        call sll_get_at(iter%tbl%buckets(iter%bucket),iter%depth,key,data_container,found)
        if (iter%depth > node_depth(iter%tbl%buckets(iter%bucket))) then
            iter%bucket = iter%bucket + 1
            iter%depth = 1
        else
            iter%depth = iter%depth + 1
        end if
        end do

        if (found) then
            call data_container%get_ptr(raw=data)
            select type(data)
                type is(network_t)
                ptr=>data
            end select

        end if

        end function 
    
end module
