module mtln_types_mod

    implicit none

    ! networks

    type :: terminal_node_t
        type(cable_t), pointer :: belongs_to_cable => null()
        integer :: conductor_in_cable
        character(len=:), allocatable :: side ! initial or end
    end type

    type :: terminal_connection_t
        type(terminal_node_t), dimension(:), allocatable :: nodes
    end type

    type :: terminal_network_t
        type(terminal_connection_t), dimension(:), allocatable :: connections
    end type

    ! bundles

    type, public :: transfer_impedance_per_meter_t
        real :: inductive_term
        real :: resistive_term
        complex, dimension(:), allocatable :: poles, residues
        ! poles and residues
        character (len=:), allocatable :: direction
    end type

    type :: connector_t
        real, dimension(:), allocatable :: resistances
        type(transfer_impedance_per_meter_t) :: transfer_impedance_per_meter
    end type

    type, public :: cable_t
        character (len=:), allocatable :: name
        real, allocatable, dimension(:,:) :: resistance_per_meter
        real, allocatable, dimension(:,:) :: capacitance_per_meter
        real, allocatable, dimension(:,:) :: inductance_per_meter
        real, allocatable, dimension(:,:) :: conductance_per_meter
        real, allocatable, dimension(:,:) :: step_size
        type(transfer_impedance_per_meter_t) :: transfer_impedance
        type(cable_t), pointer :: parent_cable => null()
        integer :: conductor_in_parent
        type(connector_t), pointer :: initial_connector => null()
        type(connector_t), pointer :: end_connector => null()
        real, allocatable, dimension(:,:) :: node_positions !!!
    end type

    type, public :: parsed_t
        type(cable_t), dimension(:), allocatable :: cables
    end type

    type, public :: cable_array_t
        type(cable_t), dimension(:), allocatable :: cables
    end type

    type, public :: cable_bundle_t
        type(cable_array_t), dimension(:), allocatable :: levels
    end type




end module