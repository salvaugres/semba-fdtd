module preprocess_mod

    use mtln_types_mod
    use mtl_bundle_mod
    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    use network_bundle_mod
    implicit none
    !preprocess: gets parser info, generates bundles and networks

    type, public :: preprocess_t
        class(mtl_bundle_t), dimension(:), allocatable :: bundles ! dict{name:bundle} of MLTD
        type(network_bundle_t), dimension(:), allocatable :: networks !lista de NetworkD, no de network

        real, private :: priv_real
    
    end type

    interface preprocess_t
        module procedure preprocessCtor
    end interface


contains

    function preprocessCtor(parsed) result(res)
        type(parsed_t) :: parsed
        type(preprocess_t) :: res
        !ToDo
    end function

end module