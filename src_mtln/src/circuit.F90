module circuit_mod

    ! use iso_c_binding
    use ngspice_interface_mod
    implicit none

    type nodes_t
        real(kind=8), allocatable :: voltages(:)
        real(kind=8), allocatable :: indices(:)
        character(len=10), allocatable :: names(:)
    end type nodes_t

    type, public :: circuit_t
    character (len=:), allocatable :: name
    real :: time = 0.0, dt = 0.0
    logical :: errorFlag = .false.
    type(nodes_t) :: nodes   

    contains
        procedure :: init
        procedure :: run
        procedure :: step
        procedure :: resume
        procedure :: loadNetlist
        procedure :: print
        procedure :: command

    end type circuit_t

contains

    subroutine init(this)
        class(circuit_t) :: this
        integer :: res

        res = ngSpice_Init(c_funloc(SendChar), &
                           c_funloc(SendStat), & 
                           c_funloc(ControlledExit), & 
                           c_funloc(SendData), &
                           c_funloc(SendInitData), &
                           c_funloc(BGThreadRunning), &
                           this%nodes)
  

        write(*,*) 'Init'
    end subroutine

    subroutine loadNetlist(this, netlist)
        class(circuit_t) :: this
        character(len=*), intent(in) :: netlist
        integer :: res
        res = ngSpice_Command('source ' // netlist // c_null_char)
    end subroutine

    subroutine step(this)
        class(circuit_t) :: this
        real :: stopTime
        character(20) :: realString
        stopTime = this%time + this%dt
        write(realString, '(E10.2)') stopTime

        ! call this%command('.tran '// realString //' '// realString )

        call this%command('stop when time = '//realString)
        call this%run()
        if (this%time == 0) then
            call this%run()
        else
            call this%resume()
        end if

    end subroutine

    subroutine run(this)
        class(circuit_t) :: this
        integer :: out
        out = ngSpice_Command('run ' // c_null_char)
    end subroutine

    subroutine resume(this)
        class(circuit_t) :: this
        integer :: out
        out = ngSpice_Command('resume ' // c_null_char)
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

    integer(c_int) function SendChar(output, id, nodes)
        type(c_ptr), value, intent(in) :: output
        integer(c_int), intent(in), value :: id
        type(nodes_t) :: nodes
        character(len=:), pointer :: f_output
        character(len=:), allocatable :: string
        
        SendChar = 0
        call c_f_pointer(output, f_output)
        string = f_output(1:index(f_output, c_null_char)-1)
        write(*,*) 'SendChar: ', trim(string)
        if (index('stderror Error:', string) /= 0) then
            SendChar = 1
        end if
    end function

    integer(c_int) function SendStat(status, id, nodes)
        type(c_ptr), value, intent(in) :: status
        integer(c_int), intent(in), value :: id
        ! type(c_ptr), value :: nodes
        type(nodes_t) :: nodes
        character(len=:), pointer :: f_output
        character(len=:), allocatable :: string

        call c_f_pointer(status, f_output)
        string = f_output(1:index(f_output, c_null_char)-1)
        write(*,*) 'SendStat: ', trim(string)
    end function

    integer(c_int) function ControlledExit(status, unloadDll, exitOnQuit, id, nodes)
        logical(c_bool), intent(in) :: unloadDll, exitOnQuit
        integer(c_int), intent(in), value :: status, id
        ! type(c_ptr), value :: nodes
        type(nodes_t) :: nodes

        integer :: res
        if (exitOnQuit .eqv. .true.) then
            write(*,*) 'ControlledExit: Returned form quit with exit status ', status
            call exit(status)
        else if (unloadDll .eqv. .true.) then 
            write(*,*) "ControlledExit: Unloading ngspice inmmediately is not possible"
            write(*,*) "ControlledExit: Can we recover?"
        else
            write(*,*) "ControlledExit: Unloading ngspice is not possible"
            write(*,*) "ControlledExit: Can we recover? Send 'quit' command to ngspice"
            res = ngSpice_Command("quit 5")
        end if

    end function

    function getName(cName) result(res)
        type(c_ptr) :: cName
        character(len=100) :: res
        character, pointer :: f_output(:) => null()
        integer :: i
        call c_f_pointer(cName, f_output,[100])
        do i = 1,100
            res(i:i) = f_output(i)
        enddo
        res = res(1:index(res, c_null_char)-1)
    end function

    integer(c_int) function SendData(data, numberOfStructs, id, nodes)
        type(c_ptr), value, intent(in) :: data
        type(nodes_t) :: nodes
        integer(c_int), value :: numberOfStructs, id

        type(vecValuesAll), pointer :: valuesAll
        type(c_ptr), pointer :: values(:)
        type(vecValuesArray), allocatable :: vecsaPtr(:) ! array of pointers to type(c_ptr)
        integer :: i

        write(*,*) 'SendData begin'
        
        call c_f_pointer(data, valuesAll) 
        call c_f_pointer(valuesAll%vecsa, values, [valuesAll%vecCount])
        allocate(vecsaPtr(valuesAll%vecCount))

        if (.not.allocated(nodes%voltages)) then 
            allocate(nodes%voltages(valuesAll%vecCount))
            allocate(nodes%indices(valuesAll%vecCount))
            allocate(nodes%names(valuesAll%vecCount))
        end if  
        
        do i = 1, valuesAll%vecCount
            call c_f_pointer(values(i), vecsaPtr(i)%vecValuesPtr)
            nodes%voltages(i) = vecsaPtr(i)%vecValuesPtr%cReal
            nodes%names(i) = trim(getName(vecsaPtr(i)%vecValuesPtr%name))
        end do
        write(*,*) trim(getName(vecsaPtr(4)%vecValuesPtr%name)), vecsaPtr(4)%vecValuesPtr%cReal

        write(*,*) 'SendData end'

    end function


    integer(c_int) function SendInitData(initData, id, nodes)
        integer(c_int), value :: id
        type(vecInOfAll), pointer, intent(in) :: initData
        type(nodes_t) :: nodes
        write(*,*) 'SendInitData'
    end function

    integer(c_int) function BGThreadRunning(isBGThreadNotRunning, id, nodes)
        logical(c_bool) :: isBGThreadNotRunning
        integer(c_int), value :: id
        type(nodes_t) :: nodes
    end function


end module 