integer function test_spice() result(error_cnt)    

    use circuit_mod
    use ngspice_interface_mod
    implicit none

    type(circuit_t) :: circuit
    character(len=50) :: netlist
    character(len=:), allocatable :: plotName
    type(pVectorInfo) :: info
    type(vectorInfo), pointer :: info2
    character(len=:), pointer :: allPlots(:)
    character(len=:), pointer :: allVecs(:)

    error_cnt = 0
    netlist = '../../src_mtln/testData/netlist.cir'
    call circuit%init()
    call circuit%loadNetlist(netlist)
    call circuit%run()

    call circuit%command('print all')
    call circuit%command("display");
    call circuit%command("listing");

    ! plotName = circuit%getCurrentPlotName()
    ! write(*,*) plotName
    ! allPlots = circuit%getAllPlots()
    ! write(*,*) '1'
    allPlots => circuit%getAllPlots()
    call circuit%command("setplot dc1")
    call c_f_pointer(circuit%getVectorInfo2('v(1)'), info2)

    ! allVecs => circuit%getAllVecs(circuit%getCurrentPlotName())
    ! write(*,*) len(allVecs)
    ! write(*,*) size(allVecs)
    write(*,*) '2'

    ! info = circuit%getVectorInfo('v(1)')
    ! call c_f_pointer(info%pVectorInfo_ptr, info2)
    ! write(*,*) info2%vName

    ! write(*,*) size(allVecs)
    ! write(*,*) allVecs(1)
    ! write(*,*) allVecs(2)
    ! info2 = circuit%getVectorInfo2(allVecs(0))
    ! write(*,*) info2%vName
    ! write(*,*) info2%vLength

    ! call circuit%command('write ../../src_mtln/testData/test3.txt v(1) v(2) v(3)')
    ! call circuit%command('status')

end function