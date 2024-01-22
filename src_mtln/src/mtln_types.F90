module mtln_types

    implicit none

    type, public :: cable_t
        character (len=:), allocatable :: name
        integer, dimension(:), allocatable :: element_ids
        integer :: material_id
        integer :: initial_terminal_id, end_terminal_id
        integer :: initial_connector_id, end_connector_id
        ! contained_in : elementId or pointer to cable_t?
    end type

    type, public :: line_t
    end type

    type, public :: bundle_t
    end type

    type, public :: connector_t
    end type

    type, public :: junction_t
    end type

    type, public :: source_t
        character (len=:), allocatable :: name, type, field, magnitude_file
        integer, dimension(:), allocatable :: element_ids

    end type

    type, public :: probe_t
        real, dimension(3) :: position
        character (len=:), allocatable :: name, type, field
    end type

    type, public :: probes_t
        type(probe_t), dimension(:), pointer :: probes => null()
    end type 

end module