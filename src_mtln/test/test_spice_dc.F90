integer function test_spice_dc() result(error_cnt)    

    use circuit_mod
    use testingTools_mod
    ! use ngspice_interface_mod
    implicit none

    type(circuit_t) :: circuit
    character(len=50) :: netlist
    real :: result(4)
    integer :: i

    error_cnt = 0
    netlist = '../../src_mtln/testData/netlist_dc.cir'
    call circuit%init(netlist)
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

    write(*,*) error_cnt

end function