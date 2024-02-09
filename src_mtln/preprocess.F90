module preprocess_mod

    use mtln_types_mod
    use mtl_bundle_mod
    use network_manager_mod
    use mtl_mod!, only: mtl_t, mtl_array_t, line_bundle_t,

    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t

    
    implicit none


    type, public :: preprocess_t
        type(mtl_bundle_t), dimension(:), allocatable :: bundles
        type(network_manager_t) :: network_manager
        type(probe_t), dimension(:), allocatable :: probes
        type(fhash_tbl_t) :: conductors_before_cable
        type(fhash_tbl_t) :: cable_name_to_bundle
        real :: final_time, dt
    
    contains
        procedure :: buildMTLBundles
        procedure :: buildNetworkManager
        procedure :: buildNetwork
        procedure :: connectNodeToGround
        procedure :: connectNodes
    end type

    interface preprocess_t
        module procedure preprocess
    end interface

contains



    subroutine addConnector(line, connector, side)
        type(mtl_t), intent(inout) :: line
        type(connector_t) :: connector
        integer :: side

        integer :: i
        do i = 1, size(connector%resistances)
            line%rpul(side,i,i) = connector%resistances(i)
        end do

    end subroutine

    function conductorsInLevel(line) result(res)
        type(line_bundle_t), intent(in) :: line
        integer, dimension(:), allocatable :: res
        integer :: i,j

        allocate(res(size(line%levels)), source = 0)
        do i = 1, size(line%levels)
            do j = 1, size(line%levels(i)%lines)
                res(i) = res(i) + line%levels(i)%lines(j)%number_of_conductors
            end do
        end do
    end function

    function findConductorsBeforeCable(name, level) result(res)
        character(len=*), intent(in) :: name
        type(mtl_array_t), intent(in) :: level
        integer :: res 
        integer :: i
        res = 0
        do i = 1, size(level%lines)
            if (level%lines(i)%name /= name) then
                res = res + level%lines(i)%number_of_conductors
            else 
                return
            end if  
        end do
    end function

    function findOuterConductorNumber(line, level, conductors_in_level) result(res)
        type(mtl_t), intent(in) :: line
        type(mtl_array_t), intent(in) :: level
        integer, intent(in) :: conductors_in_level
        integer :: res
        res = findConductorsBeforeCable(line%parent_name, level) + &
              conductors_in_level + &
              line%conductor_in_parent
    end function

    function findInnerConductorRange(line, level, conductors_in_level) result(res)
        type(mtl_t), intent(in) :: line
        type(mtl_array_t), intent(in) :: level
        integer, intent(in) :: conductors_in_level
        integer, dimension(:), allocatable :: res
        integer :: k
        res = findConductorsBeforeCable(line%name, level) + & 
              conductors_in_level + &
              [(k, k = 1, line%number_of_conductors)]

    end function
    
    subroutine setBundleTransferImpedance(bundle, line)
        type(mtl_bundle_t), intent(inout) :: bundle
        type(line_bundle_t), intent(in) :: line
        integer :: i,j,k
        integer, dimension(:), allocatable :: range_in
        integer :: conductor_out
        type(transfer_impedance_per_meter_t) :: zt

        integer, dimension(:), allocatable :: conductors_in_level

        conductors_in_level = conductorsInLevel(line)
        bundle%conductors_in_level = conductors_in_level
        do i = 2, size(line%levels)
            do j = 1, size(line%levels(i)%lines)
                conductor_out = findOuterConductorNumber(line%levels(i)%lines(j), line%levels(i-1), sum(conductors_in_level(1:i-2)))
                range_in = findInnerConductorRange(line%levels(i)%lines(j), line%levels(i), sum(conductors_in_level(1:i-1)))
                call bundle%addTransferImpedance(conductor_out, range_in, line%levels(i)%lines(j)%transfer_impedance)
            end do
        end do  


    end subroutine

    subroutine mapConductorsBeforeCable(conductors_before_cable, line)
        type(fhash_tbl_t), intent(inout) :: conductors_before_cable
        type(line_bundle_t), intent(in) :: line
        integer, dimension(:), allocatable :: range_in
        integer, dimension(:), allocatable :: conductors_in_level
        integer :: i,j
        conductors_in_level = conductorsInLevel(line)
        do i = 1, size(line%levels)
            do j = 1, size(line%levels(i)%lines)
                range_in = findInnerConductorRange(line%levels(i)%lines(j), line%levels(i), sum(conductors_in_level(1:i-1)))
                if (size(range_in) /= 0) then 
                    call conductors_before_cable%set(key(line%levels(i)%lines(j)%name), range_in(1))
                else
                    call conductors_before_cable%set(key(line%levels(i)%lines(j)%name), range_in(0))
                end if
            end do
        end do  

    end subroutine

    function buildMTLBundles(this, lines) result(res)
        class(preprocess_t) :: this
        type(line_bundle_t), dimension(:), intent(in) :: lines
        type(mtl_bundle_t), dimension(:), allocatable :: res
        type(fhash_tbl_t) :: conductors_before_cable
        integer :: i

        allocate(res(size(lines)))
        do i = 1, size(lines)
            res(i) = mtldCtor(lines(i)%levels, "bundle_"//lines(i)%levels(0)%lines(0)%name)
            call setBundleTransferImpedance(res(i), lines(i))
            call mapConductorsBeforeCable(conductors_before_cable, lines(i))
            ! res(i)%addProbe()
        end do  
        this%conductors_before_cable = conductors_before_cable
    end function    

    function buildLineFromCable(cable) result(res)
        type(cable_t), intent(in) :: cable
        type(mtl_t) :: res
        integer :: conductor_in_parent = 0
        character(len=:), allocatable :: parent_name
        if (associated(cable%parent_cable)) then 
            parent_name = cable%parent_cable%name
            conductor_in_parent = cable%conductor_in_parent
        end if  

        res = mtlHomogeneous(lpul = cable%inductance_per_meter, &
                             cpul = cable%capacitance_per_meter, &
                             rpul = cable%resistance_per_meter, &
                             gpul = cable%conductance_per_meter, &
                             step_size = cable%step_size, &
                             name = cable%name, &
                             parent_name = parent_name, &
                             conductor_in_parent = conductor_in_parent, & 
                             transfer_impedance = res%transfer_impedance)

        if (associated(cable%initial_connector)) call addConnector(res, cable%initial_connector, 0)
        if (associated(cable%end_connector))     call addConnector(res, cable%initial_connector, size(res%rpul,1))
                

    end function

    function buildLineBundles(cable_bundles) result(res)
        type(cable_bundle_t), dimension(:), allocatable :: cable_bundles
        type(line_bundle_t), dimension(:), allocatable :: res
        integer :: i, j, k
        integer :: nb, nl, nc
        nb = size(cable_bundles)

        do i = 1, nb
            allocate(res(nb))
            nl = size(cable_bundles(i)%levels)
            do j = 1, nl
                allocate(res(i)%levels(nl))
                nc = size(cable_bundles(i)%levels(j)%cables)
                do k = 1, nc
                    allocate(res(i)%levels(j)%lines(nc))
                    res(i)%levels(j)%lines(nc) = buildLineFromCable(cable_bundles(i)%levels(j)%cables(k))
                end do
            end do
        end do

    end function

    function buildCableBundleFromParent(parent, cables) result(res)
        type(cable_t), intent(in) :: parent
        type(cable_t), dimension(:), intent(in) :: cables
        type(cable_array_t) :: level
        type(cable_bundle_t) :: res

        allocate(res%levels(1))
        level%cables = [parent]
        res%levels(1) = level

        do while (findNextLevel(level) /= 0)
            res%levels = [res%levels, level]
        end do

        contains
            integer function findNextLevel(curr_level)
                type(cable_array_t), intent(inout) :: curr_level
                type(cable_t), target :: tgt
                type(cable_array_t) :: next_level
                integer :: i,j
                allocate(next_level%cables(0))
                do i = 1, size(curr_level%cables) 
                    tgt = curr_level%cables(i)
                    do j = 1, size(cables)
                        if (associated(cables(j)%parent_cable, tgt)) then 
                            next_level%cables = [next_level%cables, cables(j)]
                        end if
                    end do
                end do
                curr_level = next_level
                findNextLevel = size(curr_level%cables)
            end function

    end function

    function findParentCables(cables) result(res)
        type(cable_t), dimension(:), intent(in) :: cables
        type(cable_t), dimension(:), allocatable :: res
        integer :: i
        allocate(res(0))
        do i = 1, size(cables)
            if (associated(cables(i)%parent_cable) .eqv. .false.) then 
                res = [res, cables(i)]
            end if
        end do
    end function


    function buildCableBundles(cables) result(cable_bundles)
        type(cable_t), dimension(:), intent(in) :: cables
        type(cable_bundle_t), dimension(:), allocatable :: cable_bundles
        type(cable_t), dimension(:), allocatable :: parents
        integer :: i

        allocate(cable_bundles(0))
        parents = findParentCables(cables)
        do i = 1, size(parents)
            cable_bundles = [cable_bundles, buildCableBundleFromParent(parents(i), cables)]
        end do

    end function

    function mapCablesToBundles(lines, bundles) result(res)
        type(line_bundle_t), dimension(:), allocatable :: lines
        type(mtl_bundle_t), dimension(:), allocatable :: bundles
        type(fhash_tbl_t) :: res
        integer :: i, j, k

        do i = 1, size(lines)
            do j = 1, size(lines(i)%levels)
                do k = 1, size(lines(i)%levels(j)%lines)
                    call res%set(key(lines(i)%levels(j)%lines(k)%name), value = bundles(i))
                end do
            end do
        end do

    end function

    function writeSeriesNode(node, termination, end_node) result(res)
        type(node_t), intent(in) :: node
        type(termination_t), intent(in) :: termination
        character(len=*), intent(in) :: end_node
        character(len=100), allocatable :: res(:)
        character(len=:), allocatable :: node_name
        character(20) :: sR, sL, sC, lineC

        write(sR, '(E10.2)') termination%resistance
        write(sL, '(E10.2)') termination%inductance
        write(sC, '(E10.2)') termination%capacitance
        write(lineC, '(E10.2)') node%line_c_per_meter
        allocate(res(0))
        res = [res, trim("R" // node%name // " " // node%name // " "   // node%name //"_R " // sR)]
        res = [res, trim("L" // node%name // " " // node%name // "_R " // node%name //"_L " // sL)]
        res = [res, trim("C" // node%name // " " // node%name // "_L " // end_node // sC)]
        res = [res, trim("CL" // node%name // " " // node%name // " 0 " // lineC)]
        res = [res, trim("I" // node%name // " " // " 0 " // node%name // " dc 0")]



    end function

    function writeLCpRsNode(node, termination, end_node) result(res)
        type(node_t), intent(in) :: node
        type(termination_t), intent(in) :: termination
        character(len=*), intent(in) :: end_node
        character(len=100), allocatable :: res(:)
        character(len=:), allocatable :: node_name
        character(20) :: sR, sL, sC, lineC
        
        write(sR, '(E10.2)') termination%resistance
        write(sL, '(E10.2)') termination%inductance
        write(sC, '(E10.2)') termination%capacitance
        write(lineC, '(E10.2)') node%line_c_per_meter
        
        allocate(res(0))
        res = [res, trim("L" // node%name // " " // node%name // " " // node%name //"_p " // sL)]
        res = [res, trim("C" // node%name // " " // node%name // " " // node%name //"_p " // sC)]
        res = [res, trim("R" // node%name // " " // node%name // "_p " // end_node // sR)]
        res = [res, trim("CL" // node%name // " " // node%name // " 0 " // lineC)]
        res = [res, trim("I" // node%name // " " // " 0 " // node%name // " dc 0")]

    end function

    function writeNodeDescription(node, termination, end_node) result(res)
        type(node_t), intent(in) :: node
        type(termination_t), intent(in) :: termination
        character(len=:), allocatable :: res(:)
        character(len=*), intent(in) :: end_node

        if (termination%type == "series") then 
            res = writeSeriesNode(node, termination, end_node)
        else if (termination%type == "LCpRs") then 
            res = writeLCpRsNode(node, termination, end_node)
        end if

    end function    

    function addNode(node, cable_name_to_bundle, conductors_before_cable) result(res)
        type(terminal_node_t) :: node
        type(fhash_tbl_t) :: cable_name_to_bundle
        type(fhash_tbl_t) :: conductors_before_cable
        integer :: stat
        class(*), pointer :: d
        type(node_t) :: res
        character(len=4) :: sConductor
        integer :: conductor_number

        call conductors_before_cable%get(key(node%belongs_to_cable%name), conductor_number)
        conductor_number = conductor_number + node%conductor_in_cable

        call cable_name_to_bundle%get_raw_ptr(key(node%belongs_to_cable%name), d, stat)
        if (stat /= 0) return
        write(sConductor,'(I4)') node%conductor_in_cable
        res%name = node%belongs_to_cable%name//"_"//trim(sConductor)//"_"//node%side
        select type(d)
        type is (mtl_bundle_t)
            if (node%side == "initial") then 
                res%v => d%v(conductor_number, lbound(d%v,2))
                res%i => d%i(conductor_number, lbound(d%i,2))
            else if (node%side == "end") then 
                res%v => d%v(conductor_number, ubound(d%v,2))
                res%i => d%i(conductor_number, ubound(d%i,2))
            end if
        end select

    end function

    subroutine connectNodeToGround(this, terminal_nodes, nodes, description)
        class(preprocess_t) :: this
        type(terminal_node_t), dimension(:), allocatable :: terminal_nodes
        type(node_t),  dimension(:), intent(inout) :: nodes
        character(50), dimension(:), intent(inout) :: description

        type(node_t) :: new_node
        integer :: stat
        class(*), pointer :: d
        
        new_node = addNode(terminal_nodes(1), this%cable_name_to_bundle, this%conductors_before_cable)
        nodes = [nodes, new_node]
        description = [description, writeNodeDescription(new_node, terminal_nodes(1)%termination, "0")]
    end subroutine

    subroutine connectNodes(this, terminal_nodes, nodes, description)
        class(preprocess_t) :: this
        type(terminal_node_t), dimension(:), allocatable :: terminal_nodes
        type(node_t),  dimension(:), intent(inout) :: nodes
        character(50), dimension(:), intent(inout) :: description
        type(node_t) :: new_node
        integer :: i, stat
        class(*), pointer :: d
        character(len=:), allocatable :: interior_node

        interior_node = terminal_nodes(1)%belongs_to_cable%name//"_"//terminal_nodes(2)%belongs_to_cable%name//"_inter"
        do i = 1, 2
            new_node = addNode(terminal_nodes(i), this%cable_name_to_bundle, this%conductors_before_cable)
            nodes = [nodes, new_node]
            description = [description, writeNodeDescription(new_node, terminal_nodes(i)%termination, interior_node)]
        end do
    end subroutine


    function buildNetwork(this,terminal_network) result(res)
        class(preprocess_t) :: this
        type(terminal_network_t), intent(in) :: terminal_network
        type(node_t), dimension(:), allocatable :: nodes
        character(50), dimension(:), allocatable :: description
        type(network_t) :: res
        integer :: i

        allocate(description(0))
        allocate(nodes(0))
        do i = 1, size(terminal_network%connections)
            if (size(terminal_network%connections(i)%nodes) == 1) then 
                call this%connectNodeToGround(terminal_network%connections(i)%nodes, nodes, description)
            else
                call this%connectNodes(terminal_network%connections(i)%nodes, nodes, description)
            end if
        end do

        res = networkCtor(nodes, description)
    end function

    subroutine endDescription(description)
        character(50), dimension(:), intent(inout) :: description
        description = [description, ".endc"]
        description = [description, "NULL"]
    end subroutine

    subroutine addDescription(description, nw_description)
        character(50), dimension(:), intent(inout) :: description
        character(50), dimension(:), intent(in) :: nw_description
        integer :: i
        do i = 1, size(nw_description,1)
            description = [description, nw_description(i)]
        end do
    end subroutine

    subroutine addAnalysis(description, final_time, dt)
        character(50), dimension(:), intent(inout) :: description
        real, intent(in) :: final_time, dt
        character(20) :: sTime, sdt
        write(sTime, '(E10.2)') final_time
        write(sdt, '(E10.2)') dt
        description = [description, trim(".tran "//sdt//" "//sTime)]

    end subroutine

    subroutine addSavedNodes(description, nodes)
        character(50), dimension(:), intent(inout) :: description
        type(node_t), dimension(:), intent(in) :: nodes
        character(len=:), allocatable :: saved_nodes
        integer :: i
        saved_nodes = ""
        do i = 1, size(nodes)
            saved_nodes = saved_nodes // nodes(i)%name // " "
        end do
        description = [description, trim(".save " // saved_nodes)]

    end subroutine


    function buildNetworkManager(this, terminal_networks) result(res)
        class(preprocess_t) :: this
        type(terminal_network_t), dimension(:), intent(in) :: terminal_networks
        type(network_t), dimension(:), allocatable :: networks
        type(network_manager_t) :: res
        character(50), dimension(:), allocatable :: description
        integer :: i

        allocate(networks(size(terminal_networks)))
        do i = 1, size(terminal_networks)
            networks(i) = this%buildNetwork(terminal_networks(i))
        end do
        
        allocate(description(0))
        description = [description, "* network description message"]
        do i = 1, size(networks)
            call addDescription(description, networks(i)%description)
        end do
        call addAnalysis(description, this%final_time, this%dt)
        do i = 1, size(networks)
            call addSavedNodes(description, networks(i)%nodes)
        end do
        call endDescription(description)        

        res = network_managerCtor(networks, description, this%final_time, this%dt)

    end function

    function addProbes(cable_name_to_bundle, parsed_probes) result(res)
        type(fhash_tbl_t) :: cable_name_to_bundle
        type(parsed_probe_t), dimension(:), allocatable :: parsed_probes
        type(probe_t), dimension(:), allocatable :: res
        integer :: i
        integer :: stat
        class(*), pointer :: d

        allocate(res(size(parsed_probes)))

        do i = 1, size(parsed_probes)
            call cable_name_to_bundle%get_raw_ptr(key = key(parsed_probes(i)%attached_to_cable%name), &
                                                       value = d, &
                                                       stat=stat)

            if (stat /= 0) return
            select type(d)
            type is (mtl_bundle_t)
                res = [res, d%addProbe(index = parsed_probes(i)%index, probe_type = parsed_probes(i)%type)]
            end select
        end do
    end function

    function preprocess(parsed) result(res)
        type(parsed_t), intent(in):: parsed
        type(preprocess_t) :: res
        type(fhash_tbl_t) :: cable_name_to_bundle 
        type(line_bundle_t), dimension(:), allocatable :: line_bundles
        type(cable_bundle_t), dimension(:), allocatable :: cable_bundles

        res%final_time = parsed%time_step * parsed%number_of_steps
        res%dt = parsed%time_step

        cable_bundles = buildCableBundles(parsed%cables)
        line_bundles = buildLineBundles(cable_bundles)
        res%bundles = res%buildMTLBundles(line_bundles)
        res%cable_name_to_bundle = mapCablesToBundles(line_bundles, res%bundles)
        res%probes = addProbes(res%cable_name_to_bundle, parsed%probes)
        res%network_manager = res%buildNetworkManager(parsed%networks)
        
    end function
    
end module