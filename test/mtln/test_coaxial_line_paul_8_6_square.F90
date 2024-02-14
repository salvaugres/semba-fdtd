integer function test_coaxial_line_paul_8_6_square() bind(C) result(error_cnt)    
    use mtln_solver_mod
    use testingTools_mod
    use preprocess_mod
    implicit none

    ! character(len=*), parameter :: filename = PATH_TO_TEST_DATA//'mtln/coaxial_line_paul_8_6_square.smb.json'
    character(len=*), parameter :: square_excitation = PATH_TO_TEST_DATA//'mtln/coaxial_line_paul_8_6_square.exc'
    
    type(cable_t), target :: cable
    type(terminal_node_t) :: node_left, node_right
    type(terminal_network_t) :: nw_left, nw_right
    type(parsed_t) :: parsed
    type(terminal_connection_t) :: connection_left, connection_right
    type(terminal_network_t) :: network_left, network_right

    type(parsed_probe_t) :: probe

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

    probe%attached_to_cable => cable
    probe%index = 1 
    probe%type = "voltage"


    ! parsed%networks = [network_left]
    parsed%networks = [network_left, network_right]
    parsed%cables = [cable]
    parsed%probes = [probe]
    parsed%number_of_steps = 900
    parsed%time_step = 2.0e-8
    ! pre = preprocess(parsed)

    solver = mtlnCtor(parsed)
    call solver%runUntil(18e-6)
    write(*,*) error_cnt
    ! p = Parser(file)
    ! p.run(finalTime = 18e-6)
    block
        integer :: i
        open(unit = 1, file =  'probe.txt')
        do i = 1, size(solver%bundles(1)%probes(1)%t)
            write(1,*) solver%bundles(1)%probes(1)%t(i)," ", solver%bundles(1)%probes(1)%val(i,1)
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



end function test_coaxial_line_paul_8_6_square