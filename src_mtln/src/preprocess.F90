module preprocess_mod

    use mtln_types_mod
    use mtl_bundle_mod
    use mtl_mod!, only: mtl_t, mtl_array_t, line_bundle_t,

    ! use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
    use fhash, only: fhash_tbl_t
    use fhash_key_coordinate_pair, only: fhash_key_coordinate_pair_t, key=>fhash_key
    use fhash_data_container, only: fhash_container_t
    
    use network_bundle_mod
    implicit none
    !preprocess: gets parser info, generates bundles and networks


    type, public :: preprocess_t
        class(mtl_bundle_t), dimension(:), allocatable :: bundles ! dict{name:bundle} of MLTD
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

    ! function buildMTLArray(cable_array, parsed) result(res)
    !     type(cable_array_t), intent(in) :: cable_array
    !     type(mtl_t), dimension(:), allocatable :: res
    !     type(parsed_t) :: parsed
    !     class(*), allocatable :: d
    !     integer ::stat, i
    !     allocate(res(0))
    !     do i = 1, size(cable_array%cables)
    !         call parsed%materials%get_raw(key(cable_array%cables(i)%material_id), d, stat)
    !         if (stat /= 0) return
    !         select type(d)
    !             type is(wire_t)
    !                 res = [res, buildMTLFromWire(d, cable_array%cables(i), parsed%mesh)]
    !                 !build mtl
    !             type is(multiwire_t)
    !                 res = [res, buildMTLFromMultiwire(d, parsed)]
    !                 !build mtl
    !         end select
    !     end do  
        
    ! end function

    ! function buildMTLFromWire(wire, cable, mesh) result(res)
    !     type(wire_t), intent(in) :: wire
    !     type(cable_t), intent(in) :: cable
    !     type(mesh_t), intent(in) :: mesh
    !     type(mtl_t) :: res
    !     integer :: i, stat
    !     class(*), allocatable :: poly
    !     real, dimension(3) :: init, end

    !     ! i = cable%element_ids(1)
    !     ! call mesh%elements%get_raw(key(i), poly, stat)
    !     ! if (stat /= 0) then 
    !     !     select type(poly)
    !     !     type(polyline_t)
    !     !         init = poly%coordinates(1)*mesh%grid%steps_in_direction(1)%steps
    !     !     end select
    !     ! else
    !     !     !error
    !     ! end if
    !     ! res = mtl_t(lpul = eye(1)*wire%ref_inductance_per_meter, &
    !     !             cpul = eye(1)*wire%ref_capacitance_per_meter, &
    !     !             rpul = eye(1)*wire%resistance_per_meter, & 
    !     !             gpul = eye(1)*0, &
    !     !             node_positions = ,&
    !     !             divisions =  ,&
    !     !             name = cable%name)

    ! end function 

    ! function buildMTLFromMultiwire(multiwire, parsed) result(res)
    !     type(multiwire_t), intent(in) :: multiwire
    !     type(parsed_t), intent(in) :: parsed
    !     type(mtl_t) :: res
    !     integer :: i
    ! end function 


    ! function preprocessCtor(parsed) result(res)
    !     type(parsed_t) :: parsed
    !     type(preprocess_t) :: res
    !     !ToDo
    !     type(cable_array_t), dimension(:), allocatable :: colinear_cables

    !     integer :: i, j
    !     type(bundle_t) :: bundle

    !     type(mtl_t), dimension(:), allocatable :: mtls
    !     type(mtl_array_t) :: mtl_levels
    !     type(mtl_bundle_t) :: mtl_bundle
    !     allocate(res%bundles(0))
    !     allocate(mtl_levels%levels(0))
        
    !     colinear_cables = groupColinearCables(parsed%cables, parsed%mesh%elements)
            
    !     do i = 1, size(colinear_cables)
    !         bundle = buildBundle(colinear_cables(i))
    !         do j = 1, size(bundle%levels)
    !             mtl_levels%levels = [mtl_levels%levels, buildMTLArray(bundle%levels(i),parsed)]
    !         end do
    !         mtl_bundle = mtldCtor(mtl_levels)
    !         res%bundles = [res%bundles,mtl_bundle]
    !     end do


    ! end function

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
        integer, dimension(:) :: res
        integer :: i,j,k

        allocate(res(size(line%levels)), 0)
        do i = 1, size(line%levels)
            do j = 1, size(line%levels(i))
                res(i) = res(i) + line%levels(i)%cable(j)%conductors_in_level
            end do
        end do
    end function

    function findConductorsBeforeCable(name, level) result(res)
        character(len=:), intent(in) :: name
        type(cable_array_t), intent(in) :: level
        integer :: res 
        integer :: i
        res = 0
        do i = 1, size(level%cables)
            if (level%cables(i)%name /= name) then
                res = res + level%cables(i)%number_of_conductors
            else 
                return
            end if  
        end do
    end function

    subroutine setBundleTransferImpedance(bundle, line)
        type(mtl_bundle_t), intent(inout) :: bundle
        type(line_bundle_t), intent(in) :: line
        integer :: i,j
        integer :: n_before_out, n_before_in, n_before_parent, n_before_child
        integer :: conductor_sum
        integer, dimension(:) :: range_in
        integer :: conductor_out
        type(transfer_impedance_per_meter_t) :: zt

        integer, dimension(:) :: conductors_in_level
        conductors_in_level = conductorsInLevel(line)
        character(len=:), allocatable :: name
        allocate(range_in(0), range_out(0))
        do i = 2, size(line%levels)
            conductor_sum = 0
            do j = 1, size(line%levels(i))

                name = line%levels(i)%cable(j)%parent_cable%name
                n_before_parent = findConductorsBeforeCable(name, line%levels(i-1)) + sum(conductors_in_level(1:i-2))
                conductor_out = n_before_parent + line%levels(i)%cable(j)%conductor_in_parent
                
                name = line%levels(i)%cable(j)%name
                n_before_child = findConductorsBeforeCable(name, line%levels(i)) + sum(conductors_in_level(1:i-1))
                range_in = n_before_child + [(k, k = 1, line%levels(i)%cable(j)%number_of_conductors)]

                call bundle%addTransferImpedance(conductor_out, range_in, zt)

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
        res = mtlHomogeneous(lpul = cable%inductance_per_meter, &
        cpul = cable%capacitance_per_meter, &
                    rpul = cable%resistance_per_meter, &
                    gpul = cable%conductance_per_meter, &
                    node_positions = cable%node_positions, &
                    divisions = [100], &
                    name = cable%name)
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

    function preprocess(parsed) result(res)
        type(parsed_t), intent(in):: parsed
        type(preprocess_t) :: res

        ! type(cable_bundle_t), dimension(:), allocatable :: cable_bundles ! dimension eq to number of bundles
        ! type(line_bundle_t), dimension(:), allocatable :: line_bundles

        ! cable_bundles = buildCableBundles(parsed%cables)
        ! line_bundles = buildLineBundles(cable_bundles)
        ! ! allocate(res%bundles(size(line_bundles)))
        ! res%bundles = buildMTLBundles(line_bundles)
        ! cable_bundles = 
        ! line_bundles = 
        ! allocate(res%bundles(size(line_bundles)))
        res%bundles = buildMTLBundles(buildLineBundles(buildCableBundles(parsed%cables)))
    end function
    
end module