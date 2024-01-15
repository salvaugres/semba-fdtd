integer function test_spice() result(error_cnt)    

    use ngspice_interface_mod
    implicit none
    type(circuit_t) :: circuit
    character(len=50) :: netlist
    
    
    error_cnt = 0
    ! netlist = "../testData/netlist.cir"
    ! netlist = "../netlist.cir"
    netlist = '../../src_mtln/testData/netlist.cir'
    call circuit%init(netlist)
    call circuit%command('.netlist')
    call circuit%run()
    call circuit%print()

end function