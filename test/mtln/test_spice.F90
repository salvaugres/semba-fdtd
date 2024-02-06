integer function test_spice_read_message() bind(C) result(error_cnt)    

    use circuit_mod
    use testingTools_mod
    implicit none

    type(circuit_t) :: circuit
    real :: result(4)
    character(50), dimension(:), allocatable :: input
    integer :: i
    allocate(input(0))
    input = [input, "* Multiple dc sources "]
    input = [input, "v1 1 0 dc 24"]
    input = [input, "v2 3 0 dc 15"]
    input = [input, "r1 1 2 10k"]
    input = [input, "r2 2 3 8.1k"]
    input = [input, "r3 2 0 4.7k"]
    input = [input, ".dc v1 24 24 1"]
    input = [input, ".save v(3) v(2) v(1)"]
    input = [input, ".end"]
    input = [input, "NULL"]
    ! write(*,*) input
    error_cnt = 0

    call circuit%init()
    call circuit%readInput(input)
    call circuit%run()

    result = [24.000000000000000, 9.7469741675197206, 15.000000000000000, 24.000000000000000]
    if (size(circuit%nodes%values) /= 4) then 
        error_cnt = error_cnt + 1
    end if

    do i = 1, 4                      
        if (checkNear(circuit%nodes%values(i), result(i), 0.01) .eqv. .false. ) then 
            error_cnt = error_cnt + 1
        end if
    end do

end function

integer function test_spice_dc() bind(C) result(error_cnt)    

    use circuit_mod
    use testingTools_mod
    implicit none

    type(circuit_t) :: circuit
    character(len=50) :: netlist
    real :: result(4)
    integer :: i

    error_cnt = 0
    netlist = PATH_TO_TEST_DATA//'mtln/netlist_dc.cir'
    call circuit%init(netlist=netlist)
    call circuit%run()

    result = [24.000000000000000, 9.7469741675197206, 15.000000000000000, 24.000000000000000]
    if (size(circuit%nodes%values) /= 4) then 
        error_cnt = error_cnt + 1
    end if

    do i = 1, 4                      
        if (checkNear(circuit%nodes%values(i), result(i), 0.01) .eqv. .false. ) then 
            error_cnt = error_cnt + 1
        end if
    end do

end function

integer function test_spice_ac() bind(C) result(error_cnt)    

    use circuit_mod
    use testingTools_mod
    implicit none

    type(circuit_t) :: circuit
    character(len=*, kind=c_char), parameter :: netlist= PATH_TO_TEST_DATA//c_char_'mtln/netlist_ac.cir'
    real :: finalTime
    integer :: i

    circuit%time = 0.0
    circuit%dt = 50e-6
    finalTime = 200e-6

    error_cnt = 0
    call circuit%init(netlist=netlist)
    call circuit%setStopTimes(finalTime, circuit%dt)
    do while (circuit%time < finalTime)
        call circuit%step()
        circuit%time = circuit%time + circuit%dt
        if (checkNear(circuit%getTime(), circuit%time, 0.01) .eqv. .false. ) then 
            error_cnt = error_cnt + 1
        end if
    end do

end function

integer function test_spice_current_source() bind(C) result(error_cnt)    

    use circuit_mod
    use testingTools_mod
    implicit none

    type(circuit_t) :: circuit
    character(len=50) :: netlist
    real :: finalTime, resistance
    integer :: i
    real :: current
    circuit%time = 0.0
    circuit%dt = 50e-6
    finalTime = 200e-6

    error_cnt = 0
    netlist = PATH_TO_TEST_DATA//'mtln/netlist_current_source.cir'

    resistance = 10
    current = 0.1
    call circuit%init(netlist=netlist)
    call circuit%setStopTimes(finalTime, circuit%dt)
    do while (circuit%time < finalTime)
        call circuit%updateNodeCurrent("1", current)
        call circuit%step()
        circuit%time = circuit%time + circuit%dt
        if (checkNear(circuit%getNodeVoltage("1"), current*resistance, 0.01) .eqv. .false. ) then 
            error_cnt = error_cnt + 1
        end if
        current = 2.0*current
    end do

end function