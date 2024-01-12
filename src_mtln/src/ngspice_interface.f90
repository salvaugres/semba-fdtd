module ngspice_interface_mod

    use iso_c_binding
    implicit none

    type, public :: circuit_t
        character (len=:), allocatable :: name
        real :: time, dt

    contains
        procedure :: init
        procedure :: run

    end type circuit_t

    interface
        integer(c_int) function SendChar(char, id, returnPtr) bind(C, name="SendChar")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), dimension(*), intent(in) :: char
            integer(c_int), intent(in), value :: id
            type(c_ptr), value, intent(in) :: returnPtr
        end function

        integer(c_int) function SendStat(status, id, returnPtr) bind(C, name="SendStat")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), dimension(*), intent(in) :: status
            integer(c_int), intent(in), value :: id
            type(c_ptr), value, intent(in) :: returnPtr
        end function

        integer(c_int) function ControlledExit(status, unloadDll, exitOnQuit, id, returnPtr) bind(C, name="ControlledExit")
            import :: c_int, c_ptr, c_bool
            logical(c_bool), intent(in) :: unloadDll, exitOnQuit
            integer(c_int), intent(in), value :: status, id
            type(c_ptr), value, intent(in) :: returnPtr
        end function

        integer(c_int) function SendData(structsPtr, numberOfStructs, id, returnPtr) bind(C, name="SendData")
            import :: c_int, c_ptr
            integer(c_int), value :: numberOfStructs, id
            type(c_ptr), value, intent(in) :: structsPtr, returnPtr
        end function

        integer(c_int) function SendInitData(structsPtr, id, returnPtr) bind(C, name="SendInitData")
            import :: c_int, c_ptr
            integer(c_int), value :: id
            type(c_ptr), value, intent(in) :: structsPtr, returnPtr
        end function

        integer(c_int) function BGThreadRunning(isBGThreadNotRunning, id, returnPtr) bind(C, name="BGThreadRunning")
            import :: c_int, c_ptr, c_bool
            logical(c_bool) :: isBGThreadNotRunning
            integer(c_int), value :: id
            type(c_ptr), value, intent(in) :: returnPtr
        end function

        integer(c_int) function ngSpice_Init(cbSendChar, cbSendStat, cbControlledExit, cbSendData, cbSendInitData, cbBGThreadRunning, returnPtr) bind(C, name="ngSpice_Init")
            import :: c_int, c_ptr
            procedure(SendChar), pointer :: cbSendChar
            procedure(SendStat), pointer :: cbSendStat
            procedure(ControlledExit), pointer :: cbControlledExit
            procedure(SendData), pointer :: cbSendData
            procedure(SendInitData), pointer :: cbSendInitData
            procedure(BGThreadRunning), pointer :: cbBGThreadRunning
            type(c_ptr), value, intent(in) :: returnPtr
        end function

        integer(c_int) function ngSpice_Command(command) bind(C, name="ngSpice_Command")
            import :: c_char, c_int
            character(kind=c_char), dimension(*), intent(in) :: command
        end function

        logical(c_bool) function ngSpice_running() bind(C, name="ngSpice_running")
            import :: c_bool
        end function

    end interface


contains
    subroutine init(this, netlist)
        class(circuit_t) :: this
        character(len=*), intent(in) :: netlist
        procedure(SendChar), pointer :: cbSendChar
        procedure(SendStat), pointer :: cbSendStat
        procedure(ControlledExit), pointer :: cbControlledExit
        procedure(SendData), pointer :: cbSendData
        procedure(SendInitData), pointer :: cbSendInitData
        procedure(BGThreadRunning), pointer :: cbBGThreadRunning
        procedure(ngSpice_Init), pointer :: initPtr
        type(c_ptr) :: returnPtr
        
        TYPE(c_funptr) :: cproc,c_sendchar

        integer :: res

        ! cbSendChar => f_cbSendChar
        ! cbSendStat => f_cbSendStat
        ! cbControlledExit => f_cbControlledExit
        cbSendChar => null()
        ! cbSendStat => null()
        ! cbControlledExit => null()
        ! cbSendData => null()
        ! cbSendInitData => null()
        ! cbBGThreadRunning => null()
        
        call c_f_procpointer(cproc, initPtr)
        call c_f_procpointer(c_sendchar, cbSendChar)
        res = initPtr(c_sendchar, cbSendStat, cbControlledExit, cbSendData, cbSendInitData, cbBGThreadRunning, returnPtr)
        ! res = ngSpice_Init(cbSendChar, cbSendStat, cbControlledExit, cbSendData, cbSendInitData, cbBGThreadRunning, returnPtr)
        ! res = ngSpice_Init(cbSendChar, cbSendStat, cbControlledExit, cbSendData, cbSendInitData, cbBGThreadRunning, returnPtr)
        ! res = ngSpice_Command('source ' // netlist)

    end subroutine
    
    subroutine run(this)
        class(circuit_t) :: this
        integer :: out

        out = ngSpice_Command('bg_run')

    end subroutine

    function f_cbSendChar(char, id, resPtr) result(res)
        character, dimension(*), intent(in) :: char
        integer, value, intent(in) :: id
        type(c_ptr), value, intent(in) :: resPtr
        integer :: res
        write(*,*) 'output message:  Id: ', id
        res = 0

    end function

    function f_cbSendStat(status, id, resPtr) result(res)
        character, dimension(*), intent(in) :: status
        integer, value, intent(in) :: id
        type(c_ptr), value, intent(in) :: resPtr
        integer :: res
        res = 0
    end function

    function f_cbControlledExit(status, unloadDll, exitOnQuit, id, returnPtr) result(res)
        logical(kind=1), intent(in) :: unloadDll, exitOnQuit
        integer, value, intent(in) :: status, id
        type(c_ptr), value, intent(in) :: returnPtr
        integer :: res
        write(*,*) 'status ', status, 'inmmediate ', unloadDll, 'quit ', exitOnQuit
        res = 0
    end function

    function f_cbSendData(structsPtr, numberOfStructs, id, returnPtr) result(res)
        integer, value, intent(in) :: numberOfStructs, id
        type(c_ptr), value, intent(in) :: structsPtr, returnPtr
        integer :: res
        res = 0
    end function

    function f_cbSendInitData(structsPtr, id, returnPtr) result(res)
        integer, value :: id
        type(c_ptr), value, intent(in) :: structsPtr, returnPtr
        integer :: res
        res = 0
    end function

    function f_cbBGThreadRunning(isBGThreadNotRunning, id, returnPtr) result(res)
        logical :: isBGThreadNotRunning
        integer, value :: id
        integer :: res
        type(c_ptr), value, intent(in) :: returnPtr
        res = 0
    end function

    

end module