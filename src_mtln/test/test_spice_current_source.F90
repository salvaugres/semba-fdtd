integer function test_spice_current_source() result(error_cnt)    

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
    netlist = '../../src_mtln/testData/netlist_current_source.cir'

    resistance = 10
    current = 0.1
    call circuit%init(netlist)
    call circuit%setStopTimes(finalTime, circuit%dt)
    do while (circuit%time < finalTime)
        call circuit%updateNodeCurrent("1", current)
        call circuit%step()
        circuit%time = circuit%time + circuit%dt
        ! write(*,*) circuit%getNodeVoltage("1")
        if (checkNear(circuit%getNodeVoltage("1"), current*resistance, 0.01) .eqv. .false. ) then 
            error_cnt = error_cnt + 1
        end if
        current = 2.0*current
    end do

    write(*,*) error_cnt

end function