module model_mod

    use ngspice_interface_mod
    implicit none

    type test_t
        real(kind=8), allocatable :: voltages(:)
        ! real(kind=8), allocatable :: times(:)
    end type test_t


    type, public :: circuit_t
    character (len=:), allocatable :: name
    real :: time = 0.0, dt = 0.0
    logical :: errorFlag = .false.
    

    ! type(test_t), pointer :: values   
    real(kind=8), pointer :: voltages(:)

    contains
        procedure :: init
        procedure :: run
        procedure :: loadNetlist
        procedure :: print
        procedure :: command
        procedure :: getCurrentPlotName
        procedure :: getVectorInfo
        procedure :: getVectorInfo2
        procedure :: getAllPlots
        procedure :: getAllVecs
        ! procedure :: SendChar
        ! procedure, private :: SendStat
        ! procedure, private :: SendChar
        ! procedure, private :: SendChar
        ! procedure, private :: SendChar
        ! procedure, private :: SendChar

    end type circuit_t

contains

    ! integer(c_int) function SendChar(output, id, returnPtr)
    !     ! class(circuit_t) :: this
    !     type(c_ptr), value, intent(in), optional :: output
    !     integer(c_int), intent(in), value, optional :: id
    !     type(c_ptr), value, intent(in), optional :: returnPtr
    !     character(len=:), pointer :: f_output
    !     character(len=:), allocatable :: string
        
    !     SendChar = 0
    !     call c_f_pointer(output, f_output)
    !     ! string = f_output(1:index(f_output, c_null_char)-1)
    !     ! ! string = string(index(string,'stdout'):len(string)) ! remove 'stdout'?
    !     ! write(*,*) 'SendChar: ', trim(string)
    !     ! if (index('stderror Error:', string) /= 0) then
    !     !     SendChar = 1
    !     ! end if

    ! end function


    subroutine init(this)
        class(circuit_t) :: this
        type(c_funptr) :: cSendChar
        type(c_funptr) :: cSendStat
        type(c_funptr) :: cControlledExit
        type(c_funptr) :: cSendData
        type(c_funptr) :: cSendInitData
        type(c_funptr) :: cBGThreadRunning
        ! type(*) :: returnPtr


        integer :: res


        ! cSendChar = c_null_funptr
        cSendChar = c_funloc(SendChar)
        cSendStat = c_funloc(SendStat)
        cControlledExit = c_funloc(ControlledExit)
        cSendData = c_funloc(SendData)
        cSendInitData = c_funloc(SendInitData)
        cBGThreadRunning = c_funloc(BGThreadRunning)

        ! returnPtr = c_loc(this%voltages)
        ! returnPtr = c_loc(this%values)

        res = ngSpice_Init(cSendChar, cSendStat, cControlledExit, cSendData, cSendInitData, cBGThreadRunning, c_loc(this%voltages))
        ! res = ngSpice_Command('source ' // netlist)
        ! netlist = '../../src_mtln/testData/netlist.cir'
        ! res = ngSpice_Command('source ' // netlist // c_null_char)
        ! res = ngSpice_Command('run ' // c_null_char)
  

        write(*,*) 'Init'
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
        out = ngSpice_Command(line // c_null_char)
    end subroutine

    function getCurrentPlotName(this) result(res)
        class(circuit_t) :: this
        character(len=:), allocatable :: res
        character(len=:), pointer :: ptrName
        
        call c_f_pointer(ngSpice_CurPlot(), ptrName)
        res = ptrName(1:index(ptrName, c_null_char)-1)

    end function

    function getVectorInfo(this, vectorName) result(res)
        class(circuit_t) :: this
        type(pVectorInfo) :: res
        character(len=:), pointer :: ptrName
        character(len=:), allocatable :: name
        character(len=*), intent(in) :: vectorName

        call c_f_pointer(ngSpice_CurPlot(), ptrName)
        name = ptrName(1:index(ptrName, c_null_char)-1)

        res = ngGet_Vec_Info(name//'.'//vectorName//c_null_char)
        ! res = ngGet_Vec_Info(vectorName//c_null_char)

    end function

    function getVectorInfo2(this, vectorName) result(res)
        class(circuit_t) :: this
        ! type(c_ptr) :: pv
        type(c_ptr) :: res
        ! character(len=:), pointer :: ptrName
        ! character(len=:), allocatable :: name
        character(len=*), intent(in) :: vectorName

        ! call c_f_pointer(ngSpice_CurPlot(), ptrName)
        ! name = ptrName(1:index(ptrName, c_null_char)-1)
        ! call this%command("setplot "//name);

        res = ngGet_Vec_Info2(vectorName//c_null_char)
        ! call c_f_pointer(pv, res)

    end function

    ! function getVectorInfo2(this, vectorName) result(res)
    !     class(circuit_t) :: this
    !     type(c_ptr) :: pv
    !     type(vectorInfo), pointer :: res
    !     character(len=:), pointer :: ptrName
    !     character(len=:), allocatable :: name
    !     character(len=*), intent(in) :: vectorName

    !     call c_f_pointer(ngSpice_CurPlot(), ptrName)
    !     name = ptrName(1:index(ptrName, c_null_char)-1)
    !     ! call this%command("setplot "//name);

    !     pv = ngGet_Vec_Info2(vectorName//c_null_char)
    !     call c_f_pointer(pv, res)

    ! end function

    function getAllPlots(this) result(res)
        class(circuit_t) :: this
        integer :: length
        character(len=:), pointer :: res(:)
        call c_f_pointer(ngSpice_AllPlots(), res, [1])
    end function

    function getAllVecs(this, plotName) result(res)
        class(circuit_t) :: this
        character(len=*), intent(in) :: plotName
        character(len=:), pointer :: res(:)
        character(len=:), pointer :: allPlots(:)

        ! allPlots => this%getAllPlots()
        ! call this%command('setplot '// plotName)
        call c_f_pointer(ngSpice_AllVecs(plotName//c_null_char), res, [4])

    end function

end module 