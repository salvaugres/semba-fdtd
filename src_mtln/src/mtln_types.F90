module mtln_types_mod

    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    ! use fhash_key_coordinate_pair, only: coordinate_t
    implicit none


    ! type, extends(coordinate_t) :: coordinate_tt
    !     integer, dimension(3) :: rel_positions => coordinate_t%rel_positions
    ! end type

    type, public :: element_t
        integer :: id
    end type

    type, extends(element_t) :: polyline_t
        integer, dimension(2,3) :: coordinates
        integer, dimension(2) :: coordinate_ids
    end type

    interface operator(==)
        module procedure compare_polylines
    end interface

    type, extends(element_t) :: node
        integer, dimension(1,3) :: coordinate_ids
        ! type(coordinate_t), dimension(1) :: coordinate_ids
    end type

    type, extends(element_t) :: cell
        integer, dimension(2,3) :: intervals
        ! type(coordinate_t), dimension(2) :: intervals
    end type

    
    type, public :: material_t
        character (len=:), allocatable :: name
        integer :: id
    end type
    
    type, extends(material_t) :: wire_t
        ! radius?
        real :: resistance_per_meter
        real :: ref_capacitance_per_meter, ref_inductance_per_meter
    end type
    
    type, public :: transfer_impedance_per_meter_t
        real :: inductive_term
        real :: resistive_term
        ! poles and residues
        character (len=:), allocatable :: direction
    end type

    type, extends(material_t) :: multiwire_t
        real, dimension(:,:), allocatable :: resistance_per_meter
        real, dimension(:,:), allocatable :: capacitance_per_meter
        type(transfer_impedance_per_meter_t) :: transfer_impedance_per_meter
    end type

    type, extends(material_t) :: segment_connector_t
        real, dimension(:), allocatable :: resistances
        type(transfer_impedance_per_meter_t) :: transfer_impedance_per_meter
    end type
    
    type, extends(material_t) :: terminal_t
        type(termination_t), dimension(:), allocatable :: terminations
    end type
    
    type, public :: multiwires_t
        type(multiwire_t), dimension(:), allocatable :: multiwires
    end type


    type, public :: termination_t
        character (len=:), allocatable :: type ! series, LCpRs
        real :: resistance = 0
        real :: capacitance = 1e22
        real :: inductance = 0
    end type


    type, public :: line_t
    end type

    ! type, public :: bundle_t
    ! end type
    
    type, public :: junction_t
    end type


    type, public :: source_t
        character (len=:), allocatable :: name, field, magnitude_file
        ! integer, dimension(:), allocatable :: element_ids
        type(element_t), dimension(:), allocatable :: elements

    end type

    type, public :: probe_t
        character (len=:), allocatable :: name, field
        type(element_t), dimension(:), allocatable :: elements
    end type

    type, public :: cable_t
        character (len=:), allocatable :: name
        ! type(element_t), dimension(:), allocatable :: elements
        integer, dimension(:), allocatable :: element_ids
        integer :: material_id ! id of wire, multiwire, terminal, connector
        ! type(material_t) :: material
        integer :: initial_terminal_id, end_terminal_id
        integer :: initial_connector_id, end_connector_id
        ! type(terminal_t) :: initial_terminal
        ! type(terminal_t) :: end_terminal
        ! type(segment_connector_t) :: initial_connector
        ! type(segment_connector_t) :: end_connector

        type(cable_t), pointer :: parent_cable => null()
        ! contained_in : elementId or pointer to cable_t?
    end type

    type, public :: bundle_t 
        type(cable_array_t), dimension(:), allocatable :: levels
    end type

    type, public :: cable_array_t
        type(cable_t), dimension(:), allocatable :: cables
    end type


    type, public :: parsed_t
        type(cable_t), dimension(:), allocatable :: cables
        type(source_t), dimension(:), allocatable :: sources
        type(probe_t), dimension(:), allocatable :: probes
        type(multiwire_t), dimension(:), allocatable :: multiwires
        type(wire_t), dimension(:), allocatable :: wires
        type(terminal_t), dimension(:), allocatable :: terminals
        type(segment_connector_t), dimension(:), allocatable :: connectors
        type(fhash_tbl_t) :: elements ! id : polyline, cell or node
        type(fhash_tbl_t) :: materials ! id : wire, multiwire, terminal or connector
    end type

contains

    function compare_polylines(a,b) result(res)
        type(polyline_t), intent(in) :: a,b
        logical :: res
        res = .false.
        if (all(a%coordinates(1,:) .eq. b%coordinates(1,:)) .and. &
            all(a%coordinates(2,:) .eq. b%coordinates(2,:))) then 
            res =.true.
        end if
    end function

end module