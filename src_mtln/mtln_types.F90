module mtln_types_mod

   implicit none

   integer, parameter :: TERMINATION_UNDEFINED  = -1
   integer, parameter :: TERMINATION_SHORT      =  1
   integer, parameter :: TERMINATION_OPEN       =  2
   integer, parameter :: TERMINATION_SERIES     =  3
   integer, parameter :: TERMINATION_LCpRs      =  4

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
      real :: resistance = 1e-10
      real :: inductance = 1e-12
      real :: capacitance = 1e22
   end type

   type, extends(termination_t) :: source_termination_t
      character(len=:), allocatable :: path_to_excitation
   end type

   type :: terminal_node_t
      type(cable_t), pointer :: belongs_to_cable => null()
      integer :: conductor_in_cable
      integer :: side = TERMINAL_NODE_SIDE_UNDEFINED
      class(termination_t), allocatable :: termination
   end type

   type :: terminal_connection_t
      type(terminal_node_t), dimension(:), allocatable :: nodes
   end type

   type :: terminal_network_t
      type(terminal_connection_t), dimension(:), allocatable :: connections
   end type

   type, public :: transfer_impedance_per_meter_t
      real :: inductive_term
      real :: resistive_term
      complex, dimension(:), allocatable :: poles, residues ! poles and residues
      integer :: direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
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
      real, allocatable, dimension(:) :: step_size
      type(transfer_impedance_per_meter_t) :: transfer_impedance
      type(cable_t), pointer :: parent_cable => null()
      integer :: conductor_in_parent
      type(connector_t), pointer :: initial_connector => null()
      type(connector_t), pointer :: end_connector => null()
   end type

   type :: probe_t
      type(cable_t), pointer :: attached_to_cable => null()
      integer :: index
      integer :: probe_type = PROBE_TYPE_UNDEFINED
   end type

   type, public :: mtln_t
      type(cable_t), dimension(:), allocatable :: cables
      type(terminal_network_t), dimension(:), allocatable :: networks
      type(probe_t), dimension(:), allocatable :: probes
      real :: time_step
      integer :: number_of_steps
   end type

end module
