module mtln_types_mod

    implicit none
 
    integer, parameter :: TERMINATION_UNDEFINED  = -1
    integer, parameter :: TERMINATION_SHORT      =  1
    integer, parameter :: TERMINATION_OPEN       =  2
    integer, parameter :: TERMINATION_SERIES     =  3
    integer, parameter :: TERMINATION_LCpRs      =  4
    integer, parameter :: TERMINATION_RLsCp      =  5
 
    integer, parameter :: TERMINAL_NODE_SIDE_UNDEFINED = -1
    integer, parameter :: TERMINAL_NODE_SIDE_INI       =  1
    integer, parameter :: TERMINAL_NODE_SIDE_END       =  2
 
    integer, parameter :: TRANSFER_IMPEDANCE_DIRECTION_INWARDS   =  1
    integer, parameter :: TRANSFER_IMPEDANCE_DIRECTION_OUTWARDS  =  2
    integer, parameter :: TRANSFER_IMPEDANCE_DIRECTION_BOTH      =  3
 
    integer, parameter :: PROBE_TYPE_UNDEFINED = -1
    integer, parameter :: PROBE_TYPE_VOLTAGE   =  1
    integer, parameter :: PROBE_TYPE_CURRENT   =  2
 
    type :: termination_t
       integer :: termination_type = TERMINATION_UNDEFINED
       real :: resistance = 0.0
       real :: inductance = 0.0
       real :: capacitance = 1e22
   !  contains
   !     private
   !     procedure :: termination_eq
   !     generic, public :: operator(==) => termination_eq
    end type
 
    type, extends(termination_t) :: termination_with_source_t
       character(len=:), allocatable :: path_to_excitation
   !  contains
   !     private
   !     procedure :: termination_with_source_eq
   !     generic, public :: operator(==) => termination_with_source_eq
    end type
 
    type :: terminal_node_t
       type(cable_t), pointer :: belongs_to_cable => null()
       integer :: conductor_in_cable
       integer :: side = TERMINAL_NODE_SIDE_UNDEFINED
       class(termination_t), allocatable :: termination
   !  contains
   !     private
   !     procedure :: terminal_node_eq
   !     generic, public :: operator(==) => terminal_node_eq
    end type
 
    type :: terminal_connection_t
       type(terminal_node_t), dimension(:), allocatable :: nodes
   !  contains
   !     private
   !     procedure :: terminal_connection_eq
   !     generic, public :: operator(==) => terminal_connection_eq
    end type
 
    type :: terminal_network_t
       type(terminal_connection_t), dimension(:), allocatable :: connections
   !  contains
   !     private
   !     procedure :: terminal_network_eq
   !     generic, public :: operator(==) => terminal_network_eq
    end type
 
    type, public :: transfer_impedance_per_meter_t
       real :: inductive_term
       real :: resistive_term
       complex, dimension(:), allocatable :: poles, residues ! poles and residues
       integer :: direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
   !  contains
   !     private
   !     procedure :: transfer_impedance_per_meter_eq
   !     generic, public :: operator(==) => transfer_impedance_per_meter_eq

    end type
 
    type :: connector_t
       real, dimension(:), allocatable :: resistances
       type(transfer_impedance_per_meter_t) :: transfer_impedance_per_meter
   !  contains
   !     private
   !     procedure :: connector_eq
   !     generic, public :: operator(==) => connector_eq

    end type
 
    type, public :: cable_t
       character (len=:), allocatable :: name
       real, allocatable, dimension(:,:) :: resistance_per_meter
       real, allocatable, dimension(:,:) :: capacitance_per_meter
       real, allocatable, dimension(:,:) :: inductance_per_meter
       real, allocatable, dimension(:,:) :: conductance_per_meter
       real, allocatable, dimension(:) :: step_size
       type(transfer_impedance_per_meter_t) :: transfer_impedance
       type(cable_t), pointer :: parent_cable => null()
       integer :: conductor_in_parent
       type(connector_t), pointer :: initial_connector => null()
       type(connector_t), pointer :: end_connector => null()
   !  contains
   !     private
   !     procedure :: cable_eq
   !     generic, public :: operator(==) => cable_eq

    end type
 
    type :: probe_t
       type(cable_t), pointer :: attached_to_cable => null()
       integer :: index
       integer :: probe_type = PROBE_TYPE_UNDEFINED
   !  contains
   !     private
   !     procedure :: probe_eq
   !     generic, public :: operator(==) => probe_eq
       
    end type
 
    type, public :: mtln_t
       type(cable_t), dimension(:), allocatable :: cables
       type(terminal_network_t), dimension(:), allocatable :: networks
       type(probe_t), dimension(:), allocatable :: probes
       real :: time_step
       integer :: number_of_steps
    end type
 
    interface operator(==)
         module procedure termination_eq
         module procedure termination_with_source_eq
         module procedure terminal_node_eq
         module procedure terminal_connection_eq
         module procedure terminal_network_eq
         module procedure transfer_impedance_per_meter_eq
         module procedure connector_eq
         module procedure cable_eq
         module procedure cables_eq
         module procedure probe_eq
         module procedure probes_eq
         module procedure terminal_networks_eq
         ! module procedure mtln_eq
    end interface
 
 contains

    logical function cables_eq(a,b)
       type(cable_t), dimension(:), intent(in) :: a,b
       integer :: nA, nB, i
       nA = size(a)
       nb = size(b)
       cables_eq = .true.
       if (nA /= nB) then 
         cables_eq = .false.
         return 
       end if
       do i = 1, nA 
         cables_eq = cables_eq .and. (a(i) == b(i))
       end do
    end function

    elemental logical function cable_eq(a,b)    
       type(cable_t), intent(in) :: a, b
       cable_eq = &
          (a%name == b%name) .and. &
          all(a%inductance_per_meter == b%inductance_per_meter) .and. &
          all(a%capacitance_per_meter == b%capacitance_per_meter) .and. &
          all(a%resistance_per_meter == b%resistance_per_meter) .and. &
          all(a%conductance_per_meter == b%conductance_per_meter)  .and. &
          all(a%step_size == b%step_size) .and. &
          (a%transfer_impedance == b%transfer_impedance) .and. &
          (associated(a%parent_cable, b%parent_cable)) .and. &
          (a%conductor_in_parent == b%conductor_in_parent) .and. &
          (a%initial_connector == b%initial_connector) .and. &
          (a%end_connector == b%end_connector)
    end function

    elemental logical function connector_eq(a,b)
      type(connector_t), intent(in) :: a, b
      connector_eq = &
          (all(a%resistances == b%resistances)) .and. &
          (a%transfer_impedance_per_meter == b%transfer_impedance_per_meter)
    end function

    elemental logical function transfer_impedance_per_meter_eq(a,b)
       type(transfer_impedance_per_meter_t), intent(in) :: a, b
       transfer_impedance_per_meter_eq = &
          (a%inductive_term == b%inductive_term) .and. &
          (a%resistive_term == b%resistive_term) .and. &
          (all(a%poles == b%poles)) .and. &
          (all(a%residues == b%residues)) .and. &
          (a%direction == b%direction)
    end function

    elemental logical function termination_eq(a, b)
       class(termination_t), intent(in) :: a
       type(termination_t), intent(in) :: b
       termination_eq = &
          (a%termination_type == b%termination_type) .and. &
          (a%resistance == b%resistance) .and. &
          (a%inductance == b%inductance) .and. &
          (a%capacitance == b%capacitance)
    end function
 
    elemental logical function termination_with_source_eq(a, b)
       class(termination_with_source_t), intent(in) :: a
       type(termination_with_source_t), intent(in) :: b
         termination_with_source_eq = &
            a%termination_t == b%termination_t .and. &
            a%path_to_excitation == b%path_to_excitation
    end function
 
    logical function probes_eq(a,b)
       type(probe_t), dimension(:), intent(in) :: a,b
       integer :: nA, nB, i
       nA = size(a)
       nb = size(b)
       probes_eq = .true.
       if (nA /= nB) then 
         probes_eq = .false.
         return 
       end if
       do i = 1, nA 
         probes_eq = probes_eq .and. (a(i) == b(i))
       end do
    end function

    elemental logical function probe_eq(a,b)
      type(probe_t), intent(in) :: a,b
      probe_eq = &
         (a%index == b%index) .and. &
         (a%probe_type == b%probe_type) .and. &
         (associated(a%attached_to_cable, b%attached_to_cable))
    end function

    elemental logical function terminal_node_eq(a, b)
    type(terminal_node_t), intent(in) :: a, b

      terminal_node_eq = &
         (associated(a%belongs_to_cable, b%belongs_to_cable)) .and. &
         (a%conductor_in_cable == b%conductor_in_cable) .and. &
         (a%side == b%side) .and. &
         (a%termination == b%termination)
    end function

    elemental logical function terminal_connection_eq(a,b)
      type(terminal_connection_t), intent(in) :: a,b
      terminal_connection_eq = &
         all(a%nodes == b%nodes) ! all should not be necessary, terminal_node_eq is elemental 
    end function

    logical function terminal_networks_eq(a,b)
      type(terminal_network_t), dimension(:), intent(in) :: a,b
      integer :: nA, nB, i
      nA = size(a)
      nb = size(b)
      terminal_networks_eq = .true.
      if (nA /= nB) then 
         terminal_networks_eq = .false.
         return 
      end if
      do i = 1, nA 
         terminal_networks_eq = terminal_networks_eq .and. (a(i) == b(i))
      end do
    end function

    elemental logical function terminal_network_eq(a,b)
      type(terminal_network_t), intent(in) :: a,b 
      terminal_network_eq = &
         all(a%connections == b%connections)
    end function

 end module
