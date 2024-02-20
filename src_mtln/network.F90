module network_mod

    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    use mtl_bundle_mod
    use mtln_types_mod
    use circuit_mod, only: string_t
    implicit none

    type values_t
        real, pointer :: v
        real, pointer :: i
    end type

    type node_t
        type(values_t) :: values
        real :: is_now = 0
        ! real :: is_prev = 0
        character(len=:), allocatable :: name
        character(len=:), allocatable :: source
        real :: line_c_per_meter
        real :: r_from_line
        real :: step
        ! real, pointer :: v
        ! real, pointer :: i
    end type


    type, public :: network_t
        integer :: number_of_nodes = 0
        type(node_t), dimension(:), allocatable :: nodes
        character(256), dimension(:), allocatable :: description
        ! type(circuit_t), pointer :: circuit
    contains

    end type network_t

    interface network_t
        module procedure networkCtor
    end interface


contains

    integer function countNodes(connections)
        type(terminal_connection_t), dimension(:), allocatable :: connections
        integer :: i
        countNodes = 0
        do i = 1, size(connections)
            countNodes = countNodes + size(connections(i)%nodes)
        end do
    end function

    function networkCtor(nodes, description) result(res)
        type(node_t), dimension(:), intent(in) :: nodes
        character(*), dimension(:), intent(in) :: description
        type(string_t), dimension(:), allocatable :: names
        integer :: i
        type(network_t) :: res

        res%nodes = nodes
        res%description = description
        res%number_of_nodes = size(nodes)

        allocate(names(size(nodes)))
        do i = 1, size(nodes)
            names(i)%name = nodes(i)%name
            names(i)%length = len(nodes(i)%name)
        end do

    end function



end module network_mod