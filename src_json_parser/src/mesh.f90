module mesh_mod
    use fhash, only: fhash_tbl_t, key=>fhash_key

    type, public :: element_t
        integer :: elementType
        integer :: id
        integer, dimension(:), allocatable :: coordIds
    end type

    type, public :: mesh_t
        type(fhash_tbl_t) :: tbl
        type(element_t), dimension(:), allocatable :: elements
    end type
end module