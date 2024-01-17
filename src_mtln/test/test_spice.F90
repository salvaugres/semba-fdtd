integer function test_spice() result(error_cnt)    

    use circuit_mod
    use ngspice_interface_mod
    implicit none

    type(circuit_t) :: circuit
    character(len=50) :: netlist
    character(len=:), allocatable :: plotName
    type(pVectorInfo) :: info
    type(vectorInfo), pointer :: info1, info2, info3
    character(len=:), pointer :: allPlots(:)
    character(len=:), pointer :: allVecs(:)

    real, dimension(:), pointer :: realData1, realData2, realData3

    error_cnt = 0
    netlist = '../../src_mtln/testData/netlist.cir'
    call circuit%init()
    call circuit%loadNetlist(netlist)
    call circuit%run()

    ! allPlots => circuit%getAllPlots()
    ! call circuit%command("setplot dc1")
    ! call c_f_pointer(circuit%getVectorInfo2('V(1)'), info1)
    ! call c_f_pointer(circuit%getVectorInfo2('V(2)'), info2)
    ! call c_f_pointer(circuit%getVectorInfo2('V(3)'), info3)
    ! call c_f_pointer(info1%vRealData, realData1, [info1%vLength])
    ! call c_f_pointer(info2%vRealData, realData2, [info2%vLength])
    ! call c_f_pointer(info3%vRealData, realData3, [info3%vLength])
    ! write(*,*) realData1
    ! write(*,*) realData2
    ! write(*,*) realData3

    ! call circuit%command('print all')
    ! call circuit%command("display");
    ! call circuit%command("listing");

    ! plotName = circuit%getCurrentPlotName()
    ! write(*,*) plotName
    ! allPlots = circuit%getAllPlots()
    ! write(*,*) '1'


    ! call circuit%ddd
    ! allVecs => circuit%getAllVecs(circuit%getCurrentPlotName())
    ! write(*,*) len(allVecs)
    ! write(*,*) size(allVecs)
    

    ! info = circuit%getVectorInfo('v(1)')
    ! call c_f_pointer(info%pVectorInfo_ptr, info2)
    ! write(*,*) info2%vName

    ! write(*,*) size(allVecs)
    ! write(*,*) allVecs(1)
    ! write(*,*) allVecs(2)
    ! info2 = circuit%getVectorInfo2(allVecs(0))
    ! write(*,*) info2%vName
    ! allocate(realData(info2%vLength))

    ! call circuit%command('write ../../src_mtln/testData/test3.txt v(1) v(2) v(3)')
    ! call circuit%command('status')

end function