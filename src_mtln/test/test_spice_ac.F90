integer function test_spice_ac() result(error_cnt)    

    use circuit_mod
    use testingTools_mod
    ! use ngspice_interface_mod
    implicit none

    type(circuit_t) :: circuit
    character(len=50) :: netlist
    real(kind=8) :: result(4)
    integer :: i

    error_cnt = 0
    netlist = '../../src_mtln/testData/netlist_ac.cir'
    call circuit%init()
    call circuit%loadNetlist(netlist)
    ! call circuit%command('stop when time = 100u')
    call circuit%run()
    ! call circuit%step(10)

    call circuit%command('print all')
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