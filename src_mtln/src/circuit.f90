module circuit_mod

    use ngspice_interface_mod
    implicit none

    type, public :: circuit_t
    character (len=:), allocatable :: name
    real :: time, dt
    logical :: errorFlag = .false.

    contains
        procedure :: init
        procedure :: run
        procedure :: loadNetlist
        procedure :: print
        procedure :: command
        procedure :: getCurrentPlotName
        procedure :: getCurrentVectorInfo

    end type circuit_t

contains

    subroutine init(this)
        class(circuit_t) :: this
        type(c_funptr) ::cSendChar
        type(c_funptr) ::cSendStat
        type(c_funptr) ::cControlledExit
        type(c_funptr) ::cSendData
        type(c_funptr) ::cSendInitData
        type(c_funptr) ::cBGThreadRunning
        type(c_ptr) :: returnPtr

        integer :: res
        
        ! cSendChar = c_null_funptr
        cSendChar = c_funloc(SendChar)
        cSendStat = c_funloc(SendStat)
        cControlledExit = c_funloc(ControlledExit)
        cSendData = c_funloc(SendData)
        cSendInitData = c_funloc(SendInitData)
        cBGThreadRunning = c_funloc(BGThreadRunning)

        res = ngSpice_Init(cSendChar, cSendStat, cControlledExit, cSendData, cSendInitData, cBGThreadRunning, returnPtr)
        ! res = ngSpice_Command('source ' // netlist)

    end subroutine

    subroutine loadNetlist(this, netlist)
        class(circuit_t) :: this
        character(len=*), intent(in) :: netlist
        integer :: res
        res = ngSpice_Command('source ' // netlist // c_null_char)
    end subroutine

    subroutine run(this)
        class(circuit_t) :: this
        integer :: out
        character(len=:), allocatable :: string
        out = ngSpice_Command('run ' // c_null_char)
    end subroutine

    subroutine print(this)
        class(circuit_t) :: this
        integer :: out
        out = ngSpice_Command('print all'// c_null_char)
    end subroutine

    subroutine command(this, line)
        class(circuit_t) :: this
        character(len=*), intent(in) :: line
        integer :: out
        out = ngSpice_Command(line)
    end subroutine

    function getCurrentPlotName(this) result(res)
        class(circuit_t) :: this
        character(len=:), allocatable :: res
        character(len=:), pointer :: ptrName
        
        call c_f_pointer(ngSpice_CurPlot(), ptrName)
        res = ptrName(1:index(ptrName, c_null_char)-1)

    end function

    function getCurrentVectorInfo(this) result(res)
        class(circuit_t) :: this
        type(pVectorInfo) :: res
        ! character(kind=c_char), dimension(:), pointer :: f_output
        character(len=:), pointer :: ptrName

        call c_f_pointer(ngSpice_CurPlot(), ptrName)
        res = ngGet_Vec_Info(ptrName//'.v(1)'//c_null_char)
        res = ngGet_Vec_Info('v(1)'//c_null_char)

    end function


end module 