integer function test_coaxial_line_paul_8_6_square() bind(C) result(error_cnt)    
    use mtln_solver_mod
    use testingTools_mod
    use preprocess_mod
    implicit none

    ! character(len=*), parameter :: filename = PATH_TO_TEST_DATA//'mtln/coaxial_line_paul_8_6_square.smb.json'
    character(len=*), parameter :: square_excitation = PATH_TO_TEST_DATA//'mtln/coaxial_line_paul_8_6_block_square.exc'
    
    type(cable_t), target :: cable
    type(terminal_node_t) :: node_left, node_right
    type(terminal_network_t) :: nw_left, nw_right
    type(parsed_t) :: parsed
    type(terminal_connection_t) :: connection_left, connection_right
    type(terminal_network_t) :: network_left, network_right

    type(parsed_probe_t) :: probe_v, probe_i

    type(preprocess_t) :: pre
    type(mtln_t) :: solver
    real,dimension(1,1) :: lpul = 0.25e-6
    real,dimension(1,1) :: gpul = 0.0
    real,dimension(1,1) :: rpul = 0.0
    real,dimension(1,1) :: cpul = 100.0e-12

    character(20) :: charR, charL, charC, lineC

    error_cnt = 0

    cable%name = "wire0"
    cable%inductance_per_meter = lpul
    cable%conductance_per_meter = gpul
    cable%resistance_per_meter = rpul
    cable%capacitance_per_meter = cpul
    block
        integer :: i
        cable%step_size = [(4.0, i = 1, 100)]
    end block
    ! cable%step_size = [20.0, 20.0, 20.0, 20.0, 20.0]

    parsed%time_step = 2e-8
    ! parsed%number_of_steps = 35
    parsed%number_of_steps = 20e-6/parsed%time_step


    node_left%belongs_to_cable => cable
    node_left%conductor_in_cable = 1
    node_left%side = "initial"

    node_left%termination = source_termination_t(path_to_excitation=square_excitation, & 
                                                 type = "series", &
                                                 resistance = 150, &
                                                 inductance = 0.0, &
                                                 capacitance = 1e22)

    write(charR, '(F10.6)') node_left%termination%resistance
    write(*,*) charR

    connection_left%nodes = [node_left]

    node_right%belongs_to_cable => cable
    node_right%conductor_in_cable = 1
    node_right%side = "end"
    node_right%termination = termination_t(type="short")

    connection_right%nodes = [node_right]
    
    network_left%connections = [connection_left]
    network_right%connections = [connection_right]

    probe_v%attached_to_cable => cable
    probe_v%index = 1 
    probe_v%type = "voltage"

    probe_i%attached_to_cable => cable
    probe_i%index = 100
    probe_i%type = "current"


    ! parsed%networks = [network_left]
    parsed%networks = [network_left, network_right]
    parsed%cables = [cable]
    parsed%probes = [probe_v, probe_i]    ! pre = preprocess(parsed)

    solver = mtlnCtor(parsed)
    call solver%runUntil(parsed%time_step * parsed%number_of_steps)
    write(*,*) error_cnt
    ! p = Parser(file)
    ! p.run(finalTime = 18e-6)
    block
        integer :: i
        open(unit = 1, file =  'probes_8.6_square_alt.txt')
        do i = 1, size(solver%bundles(1)%probes(1)%t)
            write(1,*) solver%bundles(1)%probes(1)%t(i)," ", &
                       solver%bundles(1)%probes(1)%val(i,1) ," ", &
                       solver%bundles(1)%probes(2)%val(i,1)
        end do

    end block

    block
        real, dimension(:), allocatable :: start_times, end_times, expected_voltages
        integer :: i, start, end
    
        start_times = [0.1, 4.1, 6.1, 8.1, 10.1, 12.1, 14.1, 16.1]
        end_times = [3.9, 5.9, 7.9, 9.9, 11.9, 13.9, 15.9, 18.9]
        expected_voltages = [25.0, -12.5, -37.5, -18.75, 18.75, 9.375, -9.375, -4.6875]
        do i = 1, size(start_times) 
        end do
    ! for (t_start, t_end, v) in zip(start_times, end_times, check_voltages):
    !     start = np.argmin(np.abs(p.probes["v_source"].t - t_start*1e-6))
    !     end = np.argmin(np.abs(p.probes["v_source"].t - t_end*1e-6))
    !     assert np.all(np.isclose(p.probes["v_source"].val[start:end], v))
    end block



end function

integer function test_coaxial_line_paul_8_6_triangle() bind(C) result(error_cnt)    
    use mtln_solver_mod
    use testingTools_mod
    use preprocess_mod
    implicit none

    character(len=*), parameter :: square_excitation = PATH_TO_TEST_DATA//'mtln/coaxial_line_paul_8_6_triangle.exc'
    
    type(cable_t), target :: cable
    type(terminal_node_t) :: node_left, node_right
    type(terminal_network_t) :: nw_left, nw_right
    type(parsed_t) :: parsed
    type(terminal_connection_t) :: connection_left, connection_right
    type(terminal_network_t) :: network_left, network_right

    type(parsed_probe_t) :: probe_v, probe_i

    type(preprocess_t) :: pre
    type(mtln_t) :: solver
    real,dimension(1,1) :: lpul = 0.25e-6
    real,dimension(1,1) :: gpul = 0.0
    real,dimension(1,1) :: rpul = 0.0
    real,dimension(1,1) :: cpul = 100.0e-12

    character(20) :: charR, charL, charC, lineC

    error_cnt = 0

    cable%name = "wire0"
    cable%inductance_per_meter = lpul
    cable%conductance_per_meter = gpul
    cable%resistance_per_meter = rpul
    cable%capacitance_per_meter = cpul
    block
        integer :: i
        cable%step_size = [(4.0, i = 1, 100)]
    end block

    parsed%time_step = 2e-8
    parsed%number_of_steps = 20e-6/parsed%time_step


    node_left%belongs_to_cable => cable
    node_left%conductor_in_cable = 1
    node_left%side = "initial"

    node_left%termination = source_termination_t(path_to_excitation=square_excitation, & 
                                                 type = "series", &
                                                 resistance = 150, &
                                                 inductance = 0.0, &
                                                 capacitance = 1e22)

    write(charR, '(F10.6)') node_left%termination%resistance
    write(*,*) charR

    connection_left%nodes = [node_left]

    node_right%belongs_to_cable => cable
    node_right%conductor_in_cable = 1
    node_right%side = "end"
    node_right%termination = termination_t(type="short")

    connection_right%nodes = [node_right]
    
    network_left%connections = [connection_left]
    network_right%connections = [connection_right]

    probe_v%attached_to_cable => cable
    probe_v%index = 1 
    probe_v%type = "voltage"

    probe_i%attached_to_cable => cable
    probe_i%index = 100
    probe_i%type = "current"


    parsed%networks = [network_left, network_right]
    parsed%cables = [cable]
    parsed%probes = [probe_v, probe_i]

    solver = mtlnCtor(parsed)
    call solver%runUntil(parsed%time_step * parsed%number_of_steps)
    write(*,*) error_cnt
    block
        integer :: i
        open(unit = 1, file =  'probes_8.6_triangle_alt.txt')
        do i = 1, size(solver%bundles(1)%probes(1)%t)
            write(1,*) solver%bundles(1)%probes(1)%t(i)," ", &
                       solver%bundles(1)%probes(1)%val(i,1) ," ", &
                       solver%bundles(1)%probes(2)%val(i,1)
        end do

    end block

    block
        real, dimension(:), allocatable :: start_times, end_times, expected_voltages
        integer :: i, start, end
    
        start_times = [0.1, 4.1, 6.1, 8.1, 10.1, 12.1, 14.1, 16.1]
        end_times = [3.9, 5.9, 7.9, 9.9, 11.9, 13.9, 15.9, 18.9]
        expected_voltages = [25.0, -12.5, -37.5, -18.75, 18.75, 9.375, -9.375, -4.6875]
        do i = 1, size(start_times) 
        end do
    ! for (t_start, t_end, v) in zip(start_times, end_times, check_voltages):
    !     start = np.argmin(np.abs(p.probes["v_source"].t - t_start*1e-6))
    !     end = np.argmin(np.abs(p.probes["v_source"].t - t_end*1e-6))
    !     assert np.all(np.isclose(p.probes["v_source"].val[start:end], v))
    end block



end function

integer function test_2_conductor_line_paul_9_6() bind(C) result(error_cnt)    
    use mtln_solver_mod
    use testingTools_mod
    use preprocess_mod
    implicit none

    ! character(len=*), parameter :: filename = PATH_TO_TEST_DATA//'mtln/coaxial_line_paul_8_6_square.smb.json'
    character(len=*), parameter :: pulse_excitation = PATH_TO_TEST_DATA//'mtln/2_conductor_line_paul_9_6_pulse.exc'
    
    type(cable_t), target :: cable
    type(terminal_node_t) :: node_left_1, node_right_1, node_left_2, node_right_2
    type(terminal_network_t) :: nw_left, nw_right
    type(parsed_t) :: parsed
    type(terminal_connection_t) :: connection_left_1, connection_right_1, connection_left_2, connection_right_2
    type(terminal_network_t) :: network_left, network_right

    type(parsed_probe_t) :: probe_v_101, probe_v_201
    type(preprocess_t) :: pre
    type(mtln_t) :: solver
    real,dimension(2,2) :: lpul = reshape( source = [0.805756e-6, 0.538771e-6, 0.538771e-6, 1.07754e-6], shape = [2,2])
    real,dimension(2,2) :: gpul = reshape( source = [0.0, 0.0,0.0,0.0], shape = [2,2])
    real,dimension(2,2) :: rpul = reshape( source = [0.0, 0.0,0.0,0.0], shape = [2,2])
    real,dimension(2,2) :: cpul = reshape( source = [117.791e-12, -58.8956e-12, -58.8956e-12, 71.8544e-12], shape = [2,2])


    character(20) :: charR, charL, charC, lineC
    real :: final_time
    error_cnt = 0

    cable%name = "wire0"
    cable%inductance_per_meter = lpul
    cable%conductance_per_meter = gpul
    cable%resistance_per_meter = rpul
    cable%capacitance_per_meter = cpul
    block
        integer :: i
        real :: length = 0.5
        integer :: ndz = 795
        cable%step_size = [(length/ndz, i = 1, ndz)]
    end block

    final_time = 40e-9/5
    ! parsed%time_step = 4.250797024442083e-12
    parsed%number_of_steps = 10000/5
    parsed%time_step = final_time/parsed%number_of_steps
    ! parsed%number_of_steps = 10e-9/parsed%time_step


    node_left_1%belongs_to_cable => cable
    node_left_1%conductor_in_cable = 1
    node_left_1%side = "initial"
    node_left_1%termination = termination_t(type = "series", &
                                            resistance = 50, &
                                            inductance = 0.0, &
                                            capacitance = 1e22)
                                                   

    node_left_2%belongs_to_cable => cable
    node_left_2%conductor_in_cable = 2
    node_left_2%side = "initial"
    node_left_2%termination = source_termination_t(path_to_excitation=pulse_excitation, & 
                                                 type = "series", &
                                                 resistance = 50, &
                                                 inductance = 0.0, &
                                                 capacitance = 1e22)


    node_right_1%belongs_to_cable => cable
    node_right_1%conductor_in_cable = 1
    node_right_1%side = "end"
    ! node_right_1%termination = termination_t(type = "series", &
    !                                          resistance = 50, &
    !                                          inductance = 0, &
    !                                          capacitance = 1e22)
    node_right_1%termination = termination_t(type = "RLsCp", &
                                             resistance = 50, &
                                             inductance = 1e-6, &
                                             capacitance = 100e-12)

    node_right_2%belongs_to_cable => cable
    node_right_2%conductor_in_cable = 2
    node_right_2%side = "end"
    node_right_2%termination = termination_t(type = "series", &
                                             resistance = 50, &
                                             inductance = 0.0, &
                                             capacitance = 1e22)

    

    connection_left_1%nodes = [node_left_1]
    connection_left_2%nodes = [node_left_2]
    connection_right_1%nodes = [node_right_1]
    connection_right_2%nodes = [node_right_2]
    
    network_left%connections = [connection_left_1,connection_left_2]
    network_right%connections = [connection_right_1, connection_right_2]

    probe_v_101%attached_to_cable => cable
    probe_v_101%index = 1 
    probe_v_101%type = "voltage"

    probe_v_201%attached_to_cable => cable
    probe_v_201%index = 796
    probe_v_201%type = "voltage"


    ! parsed%networks = [network_left]
    parsed%networks = [network_left, network_right]
    parsed%cables = [cable]
    parsed%probes = [probe_v_101, probe_v_201]    ! pre = preprocess(parsed)

    solver = mtlnCtor(parsed)
    call solver%runUntil(parsed%time_step * parsed%number_of_steps)
    write(*,*) error_cnt
    ! p = Parser(file)
    ! p.run(finalTime = 18e-6)
    block
        integer :: i
        open(unit = 1, file =  'probes_9.6_alt_disp.txt')
        do i = 1, size(solver%bundles(1)%probes(1)%t)
            write(1,*) solver%bundles(1)%probes(1)%t(i)," ", &
                       solver%bundles(1)%probes(1)%val(i,1) ," ", &
                       solver%bundles(1)%probes(1)%val(i,2) ," ", &
                       solver%bundles(1)%probes(2)%val(i,1) ," ", &
                       solver%bundles(1)%probes(2)%val(i,2)
        end do

    end block


end function 

integer function test_2_conductor_line_paul_9_11_20ns() bind(C) result(error_cnt)    
    use mtln_solver_mod
    use testingTools_mod
    use preprocess_mod
    implicit none

    ! character(len=*), parameter :: filename = PATH_TO_TEST_DATA//'mtln/coaxial_line_paul_8_6_square.smb.json'
    character(len=*), parameter :: pulse_excitation = PATH_TO_TEST_DATA//'mtln/2_conductor_line_paul_9_11_20ns.exc'
    
    type(cable_t), target :: cable
    type(terminal_node_t) :: node_left_1, node_right_1, node_left_2, node_right_2
    type(terminal_network_t) :: nw_left, nw_right
    type(parsed_t) :: parsed
    type(terminal_connection_t) :: connection_left_1, connection_right_1, connection_left_2, connection_right_2
    type(terminal_network_t) :: network_left, network_right

    type(parsed_probe_t) :: probe_v_101, probe_v_201
    type(preprocess_t) :: pre
    type(mtln_t) :: solver
    real,dimension(2,2) :: lpul = reshape( source = [0.7485e-6, 0.5077e-6, 0.5077e-6, 1.0154e-6], shape = [2,2])
    real,dimension(2,2) :: gpul = reshape( source = [0.0, 0.0,0.0,0.0], shape = [2,2])
    real,dimension(2,2) :: rpul = reshape( source = [0.0, 0.0,0.0,0.0], shape = [2,2])
    real,dimension(2,2) :: cpul = reshape( source = [37.432e-12, -18.716e-12, -18.716e-12, 24.982e-12], shape = [2,2])

    real :: final_time

    character(20) :: charR, charL, charC, lineC

    error_cnt = 0

    cable%name = "wire0"
    cable%inductance_per_meter = lpul
    cable%conductance_per_meter = gpul
    cable%resistance_per_meter = rpul
    cable%capacitance_per_meter = cpul
    block
        integer :: i
        cable%step_size = [(1.0, i = 1, 2)]
    end block

    final_time = 200e-9
    parsed%number_of_steps = 54 ! with 53 is unstable
    parsed%time_step = final_time/parsed%number_of_steps


    node_left_1%belongs_to_cable => cable
    node_left_1%conductor_in_cable = 1
    node_left_1%side = "initial"
    node_left_1%termination = termination_t(type = "series", &
                                            resistance = 50, &
                                            inductance = 0.0, &
                                            capacitance = 1e22)
                                                   

    node_left_2%belongs_to_cable => cable
    node_left_2%conductor_in_cable = 2
    node_left_2%side = "initial"
    node_left_2%termination = source_termination_t(path_to_excitation=pulse_excitation, & 
                                                 type = "series", &
                                                 resistance = 50, &
                                                 inductance = 0.0, &
                                                 capacitance = 1e22)


    node_right_1%belongs_to_cable => cable
    node_right_1%conductor_in_cable = 1
    node_right_1%side = "end"
    node_right_1%termination = termination_t(type = "series", &
                                             resistance = 50, &
                                             inductance = 0.0, &
                                             capacitance = 1e22)

    node_right_2%belongs_to_cable => cable
    node_right_2%conductor_in_cable = 2
    node_right_2%side = "end"
    node_right_2%termination = termination_t(type = "series", &
                                             resistance = 50, &
                                             inductance = 0.0, &
                                             capacitance = 1e22)

    

    connection_left_1%nodes = [node_left_1]
    connection_left_2%nodes = [node_left_2]
    connection_right_1%nodes = [node_right_1]
    connection_right_2%nodes = [node_right_2]
    
    network_left%connections = [connection_left_1,connection_left_2]
    network_right%connections = [connection_right_1, connection_right_2]

    probe_v_101%attached_to_cable => cable
    probe_v_101%index = 1 
    probe_v_101%type = "voltage"

    parsed%networks = [network_left, network_right]
    parsed%cables = [cable]
    parsed%probes = [probe_v_101]

    solver = mtlnCtor(parsed)
    call solver%runUntil(parsed%time_step * parsed%number_of_steps)
    write(*,*) error_cnt
    ! p = Parser(file)
    ! p.run(finalTime = 18e-6)
    block
        integer :: i
        open(unit = 1, file =  'probes_9.11_20ns_alt.txt')
        do i = 1, size(solver%bundles(1)%probes(1)%t)
            write(1,*) solver%bundles(1)%probes(1)%t(i)," ", &
                       solver%bundles(1)%probes(1)%val(i,1)," ", &
                       solver%bundles(1)%probes(1)%val(i,2)
        end do

    end block


end function 

integer function test_2_conductor_line_paul_9_11_1ns() bind(C) result(error_cnt)    
    use mtln_solver_mod
    use testingTools_mod
    use preprocess_mod
    implicit none

    ! character(len=*), parameter :: filename = PATH_TO_TEST_DATA//'mtln/coaxial_line_paul_8_6_square.smb.json'
    character(len=*), parameter :: pulse_excitation = PATH_TO_TEST_DATA//'mtln/2_conductor_line_paul_9_11_1ns.exc'
    
    type(cable_t), target :: cable
    type(terminal_node_t) :: node_left_1, node_right_1, node_left_2, node_right_2
    type(terminal_network_t) :: nw_left, nw_right
    type(parsed_t) :: parsed
    type(terminal_connection_t) :: connection_left_1, connection_right_1, connection_left_2, connection_right_2
    type(terminal_network_t) :: network_left, network_right

    type(parsed_probe_t) :: probe_v_101, probe_v_201
    type(preprocess_t) :: pre
    type(mtln_t) :: solver
    real,dimension(2,2) :: lpul = reshape( source = [0.7485e-6, 0.5077e-6, 0.5077e-6, 1.0154e-6], shape = [2,2])
    real,dimension(2,2) :: gpul = reshape( source = [0.0, 0.0,0.0,0.0], shape = [2,2])
    real,dimension(2,2) :: rpul = reshape( source = [0.0, 0.0,0.0,0.0], shape = [2,2])
    real,dimension(2,2) :: cpul = reshape( source = [37.432e-12, -18.716e-12, -18.716e-12, 24.982e-12], shape = [2,2])

    real :: final_time

    character(20) :: charR, charL, charC, lineC

    error_cnt = 0

    cable%name = "wire0"
    cable%inductance_per_meter = lpul
    cable%conductance_per_meter = gpul
    cable%resistance_per_meter = rpul
    cable%capacitance_per_meter = cpul
    block
        integer :: i
        cable%step_size = [(0.02, i = 1, 100)]
    end block

    final_time = 200e-9
    parsed%number_of_steps = 2600
    parsed%time_step = final_time/parsed%number_of_steps


    node_left_1%belongs_to_cable => cable
    node_left_1%conductor_in_cable = 1
    node_left_1%side = "initial"
    node_left_1%termination = termination_t(type = "series", &
                                            resistance = 50, &
                                            inductance = 0.0, &
                                            capacitance = 1e22)
                                                   

    node_left_2%belongs_to_cable => cable
    node_left_2%conductor_in_cable = 2
    node_left_2%side = "initial"
    node_left_2%termination = source_termination_t(path_to_excitation=pulse_excitation, & 
                                                 type = "series", &
                                                 resistance = 50, &
                                                 inductance = 0.0, &
                                                 capacitance = 1e22)


    node_right_1%belongs_to_cable => cable
    node_right_1%conductor_in_cable = 1
    node_right_1%side = "end"
    node_right_1%termination = termination_t(type = "series", &
                                             resistance = 50, &
                                             inductance = 0.0, &
                                             capacitance = 1e22)

    node_right_2%belongs_to_cable => cable
    node_right_2%conductor_in_cable = 2
    node_right_2%side = "end"
    node_right_2%termination = termination_t(type = "series", &
                                             resistance = 50, &
                                             inductance = 0.0, &
                                             capacitance = 1e22)

    

    connection_left_1%nodes = [node_left_1]
    connection_left_2%nodes = [node_left_2]
    connection_right_1%nodes = [node_right_1]
    connection_right_2%nodes = [node_right_2]
    
    network_left%connections = [connection_left_1,connection_left_2]
    network_right%connections = [connection_right_1, connection_right_2]

    probe_v_101%attached_to_cable => cable
    probe_v_101%index = 1 
    probe_v_101%type = "voltage"

    parsed%networks = [network_left, network_right]
    parsed%cables = [cable]
    parsed%probes = [probe_v_101]

    solver = mtlnCtor(parsed)
    call solver%runUntil(parsed%time_step * parsed%number_of_steps)
    write(*,*) error_cnt
    ! p = Parser(file)
    ! p.run(finalTime = 18e-6)
    block
        integer :: i
        open(unit = 1, file =  'probes_9.11_1ns_alt.txt')
        do i = 1, size(solver%bundles(1)%probes(1)%t)
            write(1,*) solver%bundles(1)%probes(1)%t(i)," ", &
                       solver%bundles(1)%probes(1)%val(i,1)," ", &
                       solver%bundles(1)%probes(1)%val(i,2)
        end do

    end block


end function 