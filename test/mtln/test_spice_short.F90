integer function test_spice_short() bind(C) result(error_cnt)    

    use circuit_mod
    use testingTools_mod
    ! use ngspice_interface_mod
    implicit none

    type(circuit_t) :: circuit
    character(len=50) :: netlist
    real :: result(4), finalTime
    integer :: i

    circuit%time = 0.0
    circuit%dt = 50e-6
    finalTime = 200e-6

    error_cnt = 0
    netlist = PATH_TO_TEST_DATA//'mtln/netlist_short.cir'
    call circuit%init(netlist)
    call circuit%setStopTimes(finalTime, circuit%dt)
    do while (circuit%time < finalTime)
        call circuit%step()
        circuit%time = circuit%time + circuit%dt
    end do
    write(*,*) error_cnt

end function