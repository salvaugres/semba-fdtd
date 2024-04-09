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

   type :: segment_relative_position_t
      integer, dimension(3) ::position
   contains
      private
      procedure :: segment_relative_positions_eq
      generic, public :: operator(==) => segment_relative_positions_eq
   end type

   type :: termination_t
      integer :: termination_type = TERMINATION_UNDEFINED
      real :: resistance = 0.0
      real :: inductance = 0.0
      real :: capacitance = 1e22
   contains
      private
      procedure :: termination_eq
      generic, public :: operator(==) => termination_eq
   end type

   type, extends(termination_t) :: termination_with_source_t
      character(len=:), allocatable :: path_to_excitation
   contains
      private
      procedure :: termination_with_source_eq
      generic, public :: operator(==) => termination_with_source_eq
   end type

   type :: terminal_node_t
      type(cable_t), pointer :: belongs_to_cable => null()
      integer :: conductor_in_cable
      integer :: side = TERMINAL_NODE_SIDE_UNDEFINED
      class(termination_t), allocatable :: termination
   contains
      private
      procedure :: terminal_node_eq
      generic, public :: operator(==) => terminal_node_eq
   end type

   type :: terminal_connection_t
      type(terminal_node_t), dimension(:), allocatable :: nodes
   contains
      private
      procedure :: terminal_connection_eq
      generic, public :: operator(==) => terminal_connection_eq
      procedure, public :: add_node => terminal_connection_add_node
   end type

   type :: terminal_network_t
      type(terminal_connection_t), dimension(:), allocatable :: connections
   contains
      private
      procedure :: terminal_network_eq
      generic, public :: operator(==) => terminal_network_eq
      procedure, public :: add_connection => terminal_network_add_connection
   end type

   type, public :: transfer_impedance_per_meter_t
      real :: inductive_term = 0.0
      real :: resistive_term = 0.0
      complex, dimension(:), allocatable :: poles, residues ! poles and residues
      integer :: direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
   contains
      private
      procedure :: transfer_impedance_per_meter_eq
      generic, public :: operator(==) => transfer_impedance_per_meter_eq

   end type

   type :: connector_t
      integer :: id
      real, dimension(:), allocatable :: resistances
      type(transfer_impedance_per_meter_t) :: transfer_impedance_per_meter
   contains
      private
      procedure :: connector_eq
      generic, public :: operator(==) => connector_eq
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
      integer :: conductor_in_parent = -1
      type(connector_t), pointer :: initial_connector => null()
      type(connector_t), pointer :: end_connector => null()
      type(segment_relative_position_t), allocatable, dimension(:) :: segment_relative_positions

   contains
      private
      procedure :: cable_eq
      generic, public :: operator(==) => cable_eq
   end type

   type :: probe_t
      type(cable_t), pointer :: attached_to_cable => null()
      integer :: index
      integer :: probe_type = PROBE_TYPE_UNDEFINED
   contains
      private
      procedure :: probe_eq
      generic, public :: operator(==) => probe_eq
   end type

   type, public :: mtln_t
      type(cable_t), dimension(:), pointer :: cables
      type(terminal_network_t), dimension(:), allocatable :: networks
      type(probe_t), dimension(:), allocatable :: probes
      type(connector_t), dimension(:), pointer :: connectors
      real :: time_step
      integer :: number_of_steps
   contains
      private
      procedure :: mtln_eq
      generic, public :: operator(==) => mtln_eq
   end type


contains

   elemental logical function mtln_eq(a,b)
      class(mtln_t), intent(in) :: a,b
      mtln_eq = &
         all(a%cables == b%cables) .and. &
         all(a%probes == b%probes) .and. &
         all(a%networks == b%networks)

   end function

   elemental logical function transfer_impedance_per_meter_eq(a,b)
      class(transfer_impedance_per_meter_t), intent(in) :: a, b
      transfer_impedance_per_meter_eq = &
         (a%inductive_term == b%inductive_term) .and. &
         (a%resistive_term == b%resistive_term) .and. &
         all(a%poles == b%poles) .and. &
         all(a%residues == b%residues) .and. &
         (a%direction == b%direction)
   end function

   recursive elemental logical function cable_eq(a,b)
      class(cable_t), intent(in) :: a, b
      cable_eq = .true.
      cable_eq = cable_eq .and.  (a%name == b%name) 
      cable_eq = cable_eq .and.  all(a%inductance_per_meter == b%inductance_per_meter)
      cable_eq = cable_eq .and.  all(a%capacitance_per_meter == b%capacitance_per_meter)
      cable_eq = cable_eq .and.  all(a%resistance_per_meter == b%resistance_per_meter)
      cable_eq = cable_eq .and.  all(a%conductance_per_meter == b%conductance_per_meter)
      cable_eq = cable_eq .and.  all(a%step_size == b%step_size)
      cable_eq = cable_eq .and.  (a%transfer_impedance == b%transfer_impedance)
      cable_eq = cable_eq .and.  (a%conductor_in_parent == b%conductor_in_parent)
      cable_eq = cable_eq .and.  all(a%segment_relative_positions == b%segment_relative_positions)


      if (.not. cable_eq) then
         cable_eq = .false.
      end if

      if (.not. associated(a%parent_cable) .and. .not. associated(b%parent_cable)) then
         cable_eq = cable_eq .and. .true.
      else if ((associated(a%parent_cable) .and. .not. associated(b%parent_cable)) .or. &
         (.not. associated(a%parent_cable) .and. associated(b%parent_cable))) then
         cable_eq = cable_eq .and. .false.
      else
         cable_eq = cable_eq .and. (a%parent_cable == b%parent_cable)
      end if

      if (.not. cable_eq) then
         cable_eq = .false.
      end if

      if (.not. associated(a%initial_connector) .and. .not. associated(b%initial_connector)) then
         cable_eq = cable_eq .and. .true.
      else if ((associated(a%initial_connector) .and. .not. associated(b%initial_connector)) .or. &
         (.not. associated(a%initial_connector) .and. associated(b%initial_connector))) then
         cable_eq = cable_eq .and. .false.
      else
         cable_eq = cable_eq .and. (a%initial_connector == b%initial_connector)
      end if
      if (.not. cable_eq) then
         cable_eq = .false.
      end if

      if (.not. associated(a%end_connector) .and. .not. associated(b%end_connector)) then
         cable_eq = cable_eq .and. .true.
      else if ((associated(a%end_connector) .and. .not. associated(b%end_connector)) .or. &
         (.not. associated(a%end_connector) .and. associated(b%end_connector))) then
         cable_eq = cable_eq .and. .false.
      else
         cable_eq = cable_eq .and. (a%end_connector == b%end_connector)
      end if
      if (.not. cable_eq) then
         cable_eq = .false.
      end if

   end function

   elemental logical function connector_eq(a,b)
      class(connector_t), intent(in) :: a, b
      logical :: l
      connector_eq = &
         (a%id == b%id) .and. &
         (all(a%resistances == b%resistances)) .and. &
         (a%transfer_impedance_per_meter == b%transfer_impedance_per_meter)
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

   elemental logical function probe_eq(a,b)
      class(probe_t), intent(in) :: a,b
      probe_eq = &
         (a%index == b%index) .and. &
         (a%probe_type == b%probe_type)! .and. &

      if (.not. associated(a%attached_to_cable) .and. .not. associated(b%attached_to_cable)) then
         probe_eq = probe_eq .and. .true.
      else if ((associated(a%attached_to_cable) .and. .not. associated(b%attached_to_cable)) .or. &
         (.not. associated(a%attached_to_cable) .and. associated(b%attached_to_cable))) then
         probe_eq = probe_eq .and. .false.
      else
         probe_eq = probe_eq .and. (a%attached_to_cable == b%attached_to_cable)
      end if
      if (probe_eq .eqv. .false.) then
         probe_eq = .false.
      end if
   end function

   elemental logical function terminal_node_eq(a, b)
      class(terminal_node_t), intent(in) :: a, b

      terminal_node_eq = &
         (a%conductor_in_cable == b%conductor_in_cable) .and. &
         (a%side == b%side) .and. &
         (a%termination == b%termination)

      if (.not. associated(a%belongs_to_cable) .and. .not. associated(b%belongs_to_cable)) then
         terminal_node_eq = terminal_node_eq .and. .true.
      else if ((associated(a%belongs_to_cable) .and. .not. associated(b%belongs_to_cable)) .or. &
         (.not. associated(a%belongs_to_cable) .and. associated(b%belongs_to_cable))) then
         terminal_node_eq = terminal_node_eq .and. .false.
      else
         terminal_node_eq = terminal_node_eq .and. (a%belongs_to_cable == b%belongs_to_cable)
      end if

   end function

   elemental logical function terminal_connection_eq(a,b)
      class(terminal_connection_t), intent(in) :: a,b
      terminal_connection_eq = &
         all(a%nodes == b%nodes)
   end function

   elemental logical function terminal_network_eq(a,b)
      class(terminal_network_t), intent(in) :: a,b
      terminal_network_eq = &
         all(a%connections == b%connections)
   end function

   elemental logical function segment_relative_positions_eq(a,b)
      class(segment_relative_position_t), intent(in) :: a,b
      segment_relative_positions_eq = &
         all(a%position == b%position)
   end function

   subroutine terminal_connection_add_node(this, node)
      class(terminal_connection_t) :: this
      type(terminal_node_t) :: node
      if (.not. allocated(this%nodes))  allocate(this%nodes(0))
      this%nodes = [this%nodes, node]
   end subroutine

   subroutine terminal_network_add_connection(this, connection)
      class(terminal_network_t) :: this
      type(terminal_connection_t) :: connection
      type(terminal_connection_t), dimension(:), allocatable :: newConnections
      integer :: newConnectionsSize
      if (.not. allocated(this%connections))  allocate(this%connections(0))
      
      allocate(newConnections( size(this%connections) + 1 ) )
      newConnectionsSize = size(newConnections)
      newConnections(1:newConnectionsSize-1) = this%connections
      newConnections(newConnectionsSize) = connection
      call MOVE_ALLOC(from=newConnections, to=this%connections)
   end subroutine

end module
