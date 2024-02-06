module ngspice_interface_mod

    use iso_c_binding
    implicit none

    type, bind(c) :: vectorInfo
        type(c_ptr) :: vName
        integer(c_int) :: vType
        integer(c_short) :: vFlags
        type(c_ptr) :: vRealData !real
        type(c_ptr) :: vCompData !ngcomplex
        integer(c_int) :: vLength
    end type

    interface
        subroutine command(input) bind (C, name = "command")
            use iso_c_binding, only: c_char, c_int
            character(kind=c_char), dimension(*), intent(in) :: input
        end subroutine

        subroutine circ(input) bind(C, name = "circ")
            use iso_c_binding, only: c_ptr
            type(c_ptr), intent(in) :: input(*)
        end subroutine

        subroutine start() bind (C, name = "start")
            use iso_c_binding, only: c_int, c_ptr
            ! type(c_ptr), value :: userdata
        end subroutine

        type(c_ptr) function get_vector_info(name) bind (C, name="get_vector_info")
            use iso_c_binding, only: c_char, c_ptr
            character(kind=c_char), dimension(*), intent(in) :: name
        end function
            
        type(c_ptr) function get_all_plots() bind (C, name="get_all_plots")
            use iso_c_binding, only: c_ptr
        end function

        ! integer(c_int) function ngSpice_Init(&
        !     cbSendChar, cbSendStat, cbControlledExit, cbSendData, &
        !     cbSendInitData, cbBGThreadRunning, returnPtr) bind(C, name="ngSpice_Init")
        !     import :: c_int, c_funptr, c_ptr
        !     type(c_funptr), intent(in), value :: cbSendChar
        !     type(c_funptr), intent(in), value :: cbSendStat
        !     type(c_funptr), intent(in), value :: cbControlledExit
        !     type(c_funptr), intent(in), value :: cbSendData
        !     type(c_funptr), intent(in), value :: cbSendInitData
        !     type(c_funptr), intent(in), value :: cbBGThreadRunning
        !     type(*), intent(in) :: returnPtr
        ! end function

        ! integer(c_int) function ngSpice_Command(command) bind(C, name="ngSpice_Command")
        !     import :: c_char, c_int
        !     character(kind=c_char), dimension(*), intent(in) :: command
        ! end function

        ! logical(c_bool) function ngSpice_running() bind(C, name="ngSpice_running")
        !     import :: c_bool
        ! end function

        ! type(c_ptr) function ngSpice_CurPlot() bind(C, name="ngSpice_CurPlot")
        !     import :: c_ptr
        ! end function

        ! type(pVectorInfo) function ngGet_Vec_Info(name) bind(C, name="ngGet_Vec_Info")
        !     import :: pVectorInfo, c_char
        !     character(kind=c_char), dimension(*), intent(in) :: name
        ! end function

        ! type(c_ptr) function ngGet_Vec_Info2(name) bind(C, name="ngGet_Vec_Info")
        !     import :: c_ptr, c_char
        !     character(kind=c_char), dimension(*), intent(in) :: name
        ! end function

        ! type(c_ptr) function ngSpice_AllPlots() bind(C, name="ngSpice_AllPlots")
        !     import :: c_ptr
        ! end function

        ! type(c_ptr) function ngSpice_AllVecs(name) bind(C, name="ngSpice_AllVecs")
        !     import :: c_ptr, c_char
        !     character(kind=c_char), dimension(*), intent(in) :: name
        ! end function

        ! integer(c_int) function ngSpice_Circ(message) bind(C, name="ngSpice_Circ")
        !     import :: c_int, c_ptr, c_char
        !     type(c_ptr), intent(in) :: message(*)
        ! end function

    end interface


end module