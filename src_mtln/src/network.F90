module network_mod

    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    use mtl_bundle_mod
    use mtln_types_mod
    use circuit_mod
    implicit none

    type node_t
        real, pointer :: v
        real, pointer :: i
        character(len=:), allocatable :: name
        real :: line_c_per_meter
    end type


    type, public :: network_t
        integer :: number_of_nodes = 0
        type(node_t), dimension(:), allocatable :: nodes
        character(50), dimension(:), allocatable :: description
        type(circuit_t) :: circuit
    contains

    ! procedure, private :: connectNodeToGround
    ! procedure, private :: connectNodes
    ! procedure, private :: endDescription
    procedure, private :: updateNetworkVoltagesFromCircuit
    procedure, private :: updateCircuitCurrentsFromNetwork

    ! procedure, private :: addNodesInLine
    ! procedure, private :: addNode
    ! procedure, private :: iFactor
    ! procedure :: updateSources => network_updateSources
    ! procedure :: connectNodes
    procedure :: advanceVoltage => network_advanceVoltage
    ! procedure :: updateBundlesVoltages => network_updateBundlesVoltages
    ! procedure :: updateCurrents => network_updateCurrents
    ! procedure :: computeNWVoltageTerms => network_computeNWVoltageTerms


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

    function networkCtor(nodes, description, final_time, dt) result(res)
        type(node_t), dimension(:), allocatable :: nodes
        character(*), dimension(:), allocatable :: description
        real :: final_time, dt
        type(network_t) :: res

        res%nodes = nodes
        res%description = description
        res%number_of_nodes = size(nodes)

        call res%circuit%init()
        call res%circuit%setStopTimes(final_time, dt)
        call res%circuit%readInput(res%description)

    end function

    ! function networkCtor(terminal_network, cable_name_to_bundle, finalTime, dt) result(res)
    !     type(terminal_network_t), intent(in) :: terminal_network
    !     type(fhash_tbl_t), intent(in) :: cable_name_to_bundle 
    !     real, intent(in) :: finalTime, dt
    !     type(network_t) :: res
    !     integer :: i

    !     res%number_of_nodes = countNodes(terminal_network%connections)

        ! allocate(res%description(0))
        ! res%description = [res%description, "* network description message"]
        ! allocate(res%nodes(0))
        ! ! allocate(res%nodes(res%number_of_nodes))
        ! do i = 1, size(terminal_network%connections)
        !     if (size(terminal_network%connections(i)%nodes) == 1) then 
        !         call res%connectNodeToGround(terminal_network%connections(i)%nodes, cable_name_to_bundle)
        !     else
        !         call res%connectNodes(terminal_network%connections(i)%nodes, cable_name_to_bundle)
        !     end if
        ! end do

        ! call res%circuit%init()
        ! call res%circuit%setStopTimes(finalTime, dt)
        ! call res%endDescription(finalTime, dt)
        ! call res%circuit%readInput(res%description)

    ! end function

    ! subroutine endDescription(this, finalTime, dt)
    !     class(network_t) :: this
    !     real, intent(in) :: finalTime, dt
    !     character(len=:), allocatable :: saved_nodes
    !     character(20) :: sTime, sdt
    !     integer :: i

    !     write(sTime, '(E10.2)') finalTime
    !     write(sdt, '(E10.2)') dt
    !     saved_nodes = ""
        
    !     this%description = [this%description, ".tran "//sdt//" "//sTime]
    !     do i = 1, this%number_of_nodes
    !         saved_nodes = saved_nodes // this%nodes(i)%name // " "
    !     end do
    !     this%description = [this%description, ".save " // saved_nodes]
    !     this%description = [this%description, ".endc"]
    !     this%description = [this%description, "NULL"]

    ! end subroutine


    ! function addNode(node, dict) result(res)
    !     type(terminal_node_t) :: node
    !     type(fhash_tbl_t) :: dict
    !     integer :: stat
    !     class(*), pointer :: d
    !     type(node_t) :: res

    !     integer :: conductor_number !!! falta calcular el conductor number
        
    !     call dict%get_raw_ptr(key(node%belongs_to_cable%name), d, stat)
    !     if (stat /= 0) return
    !     res%name = node%belongs_to_cable%name//"_"//node%side
    !     select type(d)
    !     type is (mtl_bundle_t)
            

    !         if (node%side == "initial") then 
    !             res%v => d%v(conductor_number, lbound(d%v,2))
    !             res%i => d%i(conductor_number, lbound(d%i,2))
    !         else if (node%side == "end") then 
    !             res%v => d%v(conductor_number, ubound(d%v,2))
    !             res%i => d%i(conductor_number, ubound(d%i,2))
    !         end if
    !     end select
    !     ! res%line_c_per_meter = node%belongs_to_cable%capacitance_per_meter(node%conductor_in_cable,node%conductor_in_cable)

    ! end function

    ! subroutine connectNodes(this, nodes, cable_name_to_bundle)
    !     class(network_t) :: this
    !     type(terminal_node_t), dimension(:), allocatable :: nodes
    !     type(fhash_tbl_t) :: cable_name_to_bundle 
    !     type(node_t) :: new_node
    !     integer :: i, stat
    !     class(*), pointer :: d
    !     character(len=:), allocatable :: interior_node
    !     integer :: conductor_number

    !     interior_node = nodes(1)%belongs_to_cable%name//"_"//nodes(2)%belongs_to_cable%name//"_inter"

    !     do i = 1, 2
    !         new_node = addNode(nodes(i),cable_name_to_bundle)
    !         this%nodes = [this%nodes, new_node]
    !         this%description = [this%description, writeNodeDescription(new_node, nodes(i)%termination, interior_node)]
    !     end do
    !     ! lineas para circuit
    ! end subroutine

    ! subroutine connectNodeToGround(this, nodes, cable_name_to_bundle)
    !     class(network_t) :: this
    !     type(terminal_node_t), dimension(:), allocatable :: nodes
    !     type(fhash_tbl_t) :: cable_name_to_bundle 
    !     type(node_t) :: new_node
    !     integer :: stat
    !     class(*), pointer :: d
    !     ! character
    !     integer :: conductor_number
        
    !     new_node = addNode(nodes(1), cable_name_to_bundle)
    !     this%nodes = [this%nodes, new_node]
    !     ! falta las lineas para el circuit
    !     this%description = [this%description, writeNodeDescription(new_node, nodes(1)%termination, "0")]
    !     ! this%description = [this%description, writeNodeDescription(nodes(1), cable_name_to_bundle, "0")]

    ! end subroutine

    ! function writeSeriesNode(node, termination, end_node) result(res)
    !     ! type(terminal_node_t), intent(in) :: node
    !     ! type(fhash_tbl_t), intent(in) :: dict
    !     type(node_t), intent(in) :: node
    !     type(termination_t), intent(in) :: termination
    !     character(len=*), intent(in) :: end_node
    !     character(len=100), allocatable :: res(:)
    !     character(len=:), allocatable :: node_name
    !     character(20) :: sR, sL, sC, lineC

    !     write(sR, '(E10.2)') termination%resistance
    !     write(sL, '(E10.2)') termination%inductance
    !     write(sC, '(E10.2)') termination%capacitance
    !     write(lineC, '(E10.2)') node%line_c_per_meter
    !     ! write(lineC, '(E10.2)') node%belongs_to_cable%capacitance_per_meter(node%conductor_in_cable,node%conductor_in_cable)

    !     !faltan numeros para las Z
    !     ! node_name = node%belongs_to_cable%name // "_" //node%side
    !     allocate(res(0))
    !     res = [res, trim("R" // node%name // " " // node%name // " "   // node%name //"_R " // sR)]
    !     res = [res, trim("L" // node%name // " " // node%name // "_R " // node%name //"_L " // sL)]
    !     res = [res, trim("C" // node%name // " " // node%name // "_L " // end_node // sC)]
    !     res = [res, trim("CL" // node%name // " " // node%name // " 0 " // lineC)]
    !     res = [res, trim("I" // node%name // " " // " 0 " // node%name // " dc 0")]



    ! end function

    ! function writeLCpRsNode(node, termination, end_node) result(res)
    !     ! type(terminal_node_t), intent(in) :: node
    !     ! type(fhash_tbl_t), intent(in) :: dict
    !     type(node_t), intent(in) :: node
    !     type(termination_t), intent(in) :: termination
    !     character(len=*), intent(in) :: end_node
    !     character(len=100), allocatable :: res(:)
    !     character(len=:), allocatable :: node_name
    !     character(20) :: sR, sL, sC, lineC
        
    !     write(sR, '(E10.2)') termination%resistance
    !     write(sL, '(E10.2)') termination%inductance
    !     write(sC, '(E10.2)') termination%capacitance
    !     write(lineC, '(E10.2)') node%line_c_per_meter
        
    !     ! node_name = node%belongs_to_cable%name // "_" //node%side
    !     allocate(res(0))
    !     res = [res, trim("L" // node%name // " " // node%name // " " // node%name //"_p " // sL)]
    !     res = [res, trim("C" // node%name // " " // node%name // " " // node%name //"_p " // sC)]
    !     res = [res, trim("R" // node%name // " " // node%name // "_p " // end_node // sR)]
    !     res = [res, trim("CL" // node%name // " " // node%name // " 0 " // lineC)]
    !     res = [res, trim("I" // node%name // " " // " 0 " // node%name // " dc 0")]

    ! end function

    ! function writeNodeDescription(node, termination, end_node) result(res)
    !     type(node_t), intent(in) :: node
    !     type(termination_t), intent(in) :: termination
    !     ! type(terminal_node_t), intent(in) :: node
    !     ! type(fhash_tbl_t), intent(in) :: dict
    !     character(len=:), allocatable :: res(:)
    !     character(len=*), intent(in) :: end_node

    !     if (termination%type == "series") then 
    !         res = writeSeriesNode(node, termination, end_node)
    !         ! res = writeSeriesNode(node, dict, end_node)
    !     else if (termination%type == "LCpRs") then 
    !         res = writeLCpRsNode(node, termination, end_node)
    !         ! res = writeLCpRsNode(node, dict, end_node)
    !     end if

    ! end function    

    subroutine updateNetworkVoltagesFromCircuit(this)
        class(network_t) :: this
        integer :: i
        do i = 1, this%number_of_nodes
            this%nodes(i)%v = this%circuit%getNodeVoltage(this%nodes(i)%name)
        end do
    end subroutine

    subroutine updateCircuitCurrentsFromNetwork(this)
        class(network_t) :: this
        integer :: i
        do i = 1, this%number_of_nodes
            call this%circuit%updateNodeCurrent(this%nodes(i)%name, this%nodes(i)%i)
        end do
    end subroutine


    ! subroutine addNodesInLine(this)
    !     class(network_t) :: this
    ! end subroutine

    ! subroutine addNode(this)
    !     class(network_t) :: this
    ! end subroutine

    ! function iFactor(this,node) result(res)
    !     class(network_t) :: this
    !     type(fhash_tbl_t) :: node
    !     integer :: res
    !     ! TODO
    ! end function


    ! subroutine computeVoltageTerms(this)
    !     class(network_t) :: this
    !     ! TODO
    ! end subroutine

    ! subroutine network_updateSources(this, time, dt)
    !     class(network_t) :: this
    !     real, intent(in) :: time, dt
    !     ! TODO
    ! end subroutine

    subroutine network_advanceVoltage(this, dt)
        class(network_t) :: this
        real, intent(in) :: dt
        call this%circuit%step()
        this%circuit%time = this%circuit%time + this%circuit%dt
        call this%updateNetworkVoltagesFromCircuit()
    end subroutine



    ! subroutine network_updateBundlesVoltages(this, bundles)
    !     class(network_t) :: this
    !     class(fhash_tbl_t), intent(in) :: bundles
    !     ! TODO: voltages from nw to bundle
    !     ! loop over external nodes
    !     ! get pointer to line attached to node
    !     ! line[.....].v[...] = this%circuit%getNodeVoltage("node")
    ! end subroutine

    ! subroutine network_updateCurrents(this, bundles)
    !     class(network_t) :: this
    !     class(fhash_tbl_t), intent(in) :: bundles
    !     ! TODO: currents from bundle to current
    !     ! loop over external nodes
    !     ! get pointer to line attached to node
    !     ! update current source using current in bundle
    !     ! call this%circuit%updateNodeCurrent(node, current)
    ! end subroutine

    ! subroutine network_computeNWVoltageTerms(this, dt)
    !     class(network_t) :: this
    !     real, intent(in) :: dt
    !     ! TODO
    ! end subroutine 





end module network_mod