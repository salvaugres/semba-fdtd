integer function test_spice_ac() result(error_cnt)    

    use circuit_mod
    use testingTools_mod
    ! use ngspice_interface_mod
    implicit none

    type(circuit_t) :: circuit
    character(len=50) :: netlist
    real :: finalTime
    integer :: i

    circuit%time = 0.0
    circuit%dt = 50e-6
    finalTime = 200e-6

    error_cnt = 0
    netlist = '../../src_mtln/testData/netlist_ac.cir'
    call circuit%init()
    call circuit%loadNetlist(netlist)
    call circuit%setStopTimes(finalTime, circuit%dt)
    do while (circuit%time < finalTime)
        call circuit%step()
        circuit%time = circuit%time + circuit%dt
        write(*,*) 'time: ',circuit%nodes%voltages(4)
    end do

    ! call circuit%run()
    ! call circuit%resume()
    ! call circuit%resume()


    ! call circuit%command('print all')
    ! result = [24.000000000000000, 9.7469741675197206, 15.000000000000000, 24.000000000000000]
    ! if (size(circuit%nodes%voltages) /= 4) then 
    !     error_cnt = error_cnt + 1
    ! end if

    ! do i = 1, 4                      
    !     if (checkNear_dp(circuit%nodes%voltages(i), result(i), 0.01) .eqv. .false. ) then 
    !         error_cnt = error_cnt + 1
    !     end if
    ! end do

end function