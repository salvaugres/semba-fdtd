module ngspice_interface_mod

    use iso_c_binding
    implicit none

    type, public :: circuit_t
        character (len=:), allocatable :: name
        real :: time, dt

    contains
        procedure :: init
        procedure :: run
        procedure :: print
        procedure :: command
    end type circuit_t

    ! type, bind(c) :: ngcomplex
    !     real(c_double) :: real
    !     real(c_double) :: imag
    ! end type

    type, bind(c) :: vectorInfo
        character(kind=c_char), dimension(50) :: vName
        integer(c_int) :: vType
        integer(c_short) :: vFlags
        type(c_ptr) :: vRealData !real
        type(c_ptr) :: vCompData !ngcomplex
        integer(c_int) :: vLength
    end type

    type, bind(c) :: pVectorInfo
        type(c_ptr):: pVectorInfo_ptr ! type vectorInfo
        ! type(vectorInfo), pointer :: pVectorInfo_ptr
    end type

    type, bind(c) :: vecInfo
        integer(c_int) :: number
        character(kind=c_char), dimension(50) :: vecName
        logical(c_bool) :: is_real
        type(c_ptr) :: pdVec
        type(c_ptr) :: pdVecScale
    end type

    type, bind(c) :: pVecInfo
        type(c_ptr) :: pVecInfo_ptr ! vecInfo
        ! type(vecInfo), pointer :: pVecInfo_ptr
    end type
    
    type, bind(C) :: vecInOfAll
        character(kind=c_char), dimension(50) :: name
        character(kind=c_char), dimension(50) :: title
        character(kind=c_char), dimension(50) :: date
        character(kind=c_char), dimension(50) :: type
        integer(c_int) :: vecCount
        type(c_ptr) :: vecs !type pVecInfo(:)
        ! type(pVecInfo), pointer, dimension(:) :: vecs
    end type

    type, bind(C) :: pVecInOfAll
        type(c_ptr):: pVecInOfAll_ptr !type vecInOfAll
        ! type(vecInOfAll), pointer :: pVecInOfAll_ptr
    end type

    type, bind(C) :: vecValues
        character(kind=c_char), dimension(50) :: name
        real(c_double) :: cReal
        real(c_double) :: cImag
        logical(c_bool) :: isScale
        logical(c_bool) :: isComplex
    end type

    type, bind(C) :: pVecValues
        type(c_ptr) :: pVecValues_ptr ! vecValues
        ! type(vecValues), pointer :: pVecValues_ptr
    end type

    type, bind(C) :: vecValuesAll
        integer(c_int) :: vecCount
        integer(c_int) :: vecIndex
        type(c_ptr) :: vecsa ! type pVecValues(:)
        ! type(pVecValues), pointer, dimension(:) :: vecsa
    end type


    interface

        integer(c_int) function ngSpice_Init(cbSendChar, cbSendStat, cbControlledExit, cbSendData, cbSendInitData, cbBGThreadRunning, returnPtr) bind(C, name="ngSpice_Init")
            import :: c_int, c_ptr, c_funptr
            type(c_funptr), intent(in), value :: cbSendChar
            type(c_funptr), intent(in), value :: cbSendStat
            type(c_funptr), intent(in), value :: cbControlledExit
            type(c_funptr), intent(in), value :: cbSendData
            type(c_funptr), intent(in), value :: cbSendInitData
            type(c_funptr), intent(in), value :: cbBGThreadRunning
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
        res = ngSpice_Command('source ' // netlist)

    end subroutine
    
    subroutine run(this)
        class(circuit_t) :: this
        integer :: out
        out = ngSpice_Command("bg_run")
        ! do while (ngSpice_running() .eqv. .true.)
        !     write(*,*) 'running'

        ! end do
        write(*,*) 'run result ',out
    end subroutine

    subroutine print(this)
        class(circuit_t) :: this
        integer :: out
        out = ngSpice_Command("print all")
    end subroutine

    subroutine command(this, line)
        class(circuit_t) :: this
        character(len=*), intent(in) :: line
        integer :: out
        out = ngSpice_Command(line)
    end subroutine


    function SendChar(char, id, returnPtr) bind(C, name="SendChar") result(res)
        character(kind=c_char), dimension(*), intent(in) :: char
        integer(c_int), intent(in), value :: id
        type(c_ptr), value, intent(in) :: returnPtr
        integer(c_int) :: res
        res = 0
        write(*,*) 'SendChar'
        ! write(*,*) 'output message:  Id: ', id    
    end function

    integer(c_int) function SendStat(status, id, returnPtr) bind(C, name="SendStat")
        character(kind=c_char), dimension(*), intent(in) :: status
        integer(c_int), intent(in), value :: id
        type(c_ptr), value, intent(in) :: returnPtr
        write(*,*) 'SendStat'
    end function

    integer(c_int) function ControlledExit(status, unloadDll, exitOnQuit, id, returnPtr) bind(C, name="ControlledExit")
        logical(c_bool), intent(in) :: unloadDll, exitOnQuit
        integer(c_int), intent(in), value :: status, id
        type(c_ptr), value, intent(in) :: returnPtr
        write(*,*) 'ControlledExit'
    end function

    integer(c_int) function SendData(structsPtr, numberOfStructs, id, returnPtr) bind(C, name="SendData")
        integer(c_int), value :: numberOfStructs, id
        type(c_ptr), value, intent(in) :: structsPtr, returnPtr

        write(*,*) 'SendData'

    end function

    integer(c_int) function SendInitData(structsPtr, id, returnPtr) bind(C, name="SendInitData")
        integer(c_int), value :: id
        type(pVecInOfAll), dimension(:), pointer :: structsPtr
        type(c_ptr), value, intent(in) :: returnPtr



        ! structsPtr%vecCount
        write(*,*) 'SendInitData'


    end function

    integer(c_int) function BGThreadRunning(isBGThreadNotRunning, id, returnPtr) bind(C, name="BGThreadRunning")
        logical(c_bool) :: isBGThreadNotRunning
        integer(c_int), value :: id
        type(c_ptr), value, intent(in) :: returnPtr
        write(*,*) 'isBGThreadNotRunning: ', isBGThreadNotRunning, '. id: ', id
    end function


end module