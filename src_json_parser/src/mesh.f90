module mesh_mod
    use fhash, only: fhash_tbl_t, key=>fhash_key

    type, public :: node_t
        integer :: coordIds
    end type

    type, public :: polyline_t
        integer, dimension(:), allocatable :: coordIds
    end type

    type, public :: coordinate_t
        real, dimension(3) :: position
    end type

    type, public :: mesh_t
        type(fhash_tbl_t) :: coordinates, elements
    end type

    
end module