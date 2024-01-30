module preprocess_mod

    use mtln_types_mod
    use mtl_bundle_mod
    use mtl_mod!, only: mtl_t, mtl_array_t, line_bundle_t,

    use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t

    ! use fhash, only: fhash_tbl_t
    ! use fhash_key_coordinate_pair, only: fhash_key_coordinate_pair_t, key=>fhash_key
    ! use fhash_data_container, only: fhash_container_t
    
    use network_bundle_mod
    implicit none
    !preprocess: gets parser info, generates bundles and networks


    type, public :: preprocess_t
        type(mtl_bundle_t), dimension(:), allocatable :: bundles ! dict{name:bundle} of MLTD
        type(network_bundle_t), dimension(:), allocatable :: networks !lista de NetworkD, no de network
        type(transfer_impedance_t) :: external_transfer_impedance 
        real, private :: priv_real
    
    contains
        ! procedure :: buildLines
        ! procedure :: groupCollinearCables

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
        integer :: i,j,k

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
        ! allocate(res(0))
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
        ! allocate(range_in(0))
        do i = 2, size(line%levels)
            do j = 1, size(line%levels(i)%lines)
                conductor_out = findOuterConductorNumber(line%levels(i)%lines(j), line%levels(i-1), sum(conductors_in_level(1:i-2)))
                range_in = findInnerConductorRange(line%levels(i)%lines(j), line%levels(i), sum(conductors_in_level(1:i-1)))
                call bundle%addTransferImpedance(conductor_out, range_in, line%levels(i)%lines(j)%transfer_impedance)
            end do
        end do  


    end subroutine

    function buildMTLBundles(lines) result(res)
        type(line_bundle_t), dimension(:), intent(in) :: lines
        type(mtl_bundle_t), dimension(:), allocatable :: res
        integer :: i

        allocate(res(size(lines)))
        do i = 1, size(lines)
            res(i) = mtldCtor(lines(i)%levels, "bundle_"//lines(i)%levels(0)%lines(0)%name)
            call setBundleTransferImpedance(res(i), lines(i))
        end do  

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
                             node_positions = cable%node_positions, &
                             divisions = [100], &
                             name = cable%name, &
                             parent_name = parent_name, &
                             conductor_in_parent = conductor_in_parent, & 
                             transfer_impedance = res%transfer_impedance)
                    ! steps = cable%step_size, 
                    ! name = cable%name)

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

    function preprocess(parsed) result(res)
        type(parsed_t), intent(in):: parsed
        type(preprocess_t) :: res
        type(fhash_tbl_t) :: cable_name_to_bundle 
        ! type(cable_bundle_t), dimension(:), allocatable :: cable_bundles ! dimension eq to number of bundles
        type(line_bundle_t), dimension(:), allocatable :: line_bundles

        ! cable_bundles = buildCableBundles(parsed%cables)
        ! line_bundles = buildLineBundles(cable_bundles)
        ! ! allocate(res%bundles(size(line_bundles)))
        ! res%bundles = buildMTLBundles(line_bundles)
        ! cable_bundles = 
        ! line_bundles = 
        ! allocate(res%bundles(size(line_bundles)))
        line_bundles = buildLineBundles(buildCableBundles(parsed%cables))
        res%bundles = buildMTLBundles(line_bundles)
        cable_name_to_bundle = mapCablesToBundles(line_bundles, res%bundles)

    end function
    
end module