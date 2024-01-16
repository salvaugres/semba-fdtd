integer function test_spice() result(error_cnt)    

    use circuit_mod
    use ngspice_interface_mod
    implicit none

    type(circuit_t) :: circuit
    character(len=50) :: netlist
    character(len=:), allocatable :: plotName
    type(pVectorInfo) :: info

    error_cnt = 0
    netlist = '../../src_mtln/testData/netlist.cir'
    call circuit%init()
    call circuit%loadNetlist(netlist)
    call circuit%run()
    ! call circuit%command('bg_halt')

    call circuit%command('print all' // c_null_char)

    ! plotName = circuit%getCurrentPlotName()
    ! write(*,*) plotName
    info = circuit%getVectorInfo('v(1)')

    call circuit%command('write ../../src_mtln/testData/test3.txt v(1) v(2) v(3)')
    ! call circuit%command('status')

end function