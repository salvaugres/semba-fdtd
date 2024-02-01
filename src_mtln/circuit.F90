module circuit_mod

    use ngspice_interface_mod
    implicit none

    type string_t
        character(len=100) :: name
        integer :: length
    end type string_t

    type nodes_t
        real, allocatable :: values(:)
        real, allocatable :: indices(:)
        type(string_t), allocatable :: tags(:)
    
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
        procedure, private :: resume
        procedure, private :: loadNetlist
        procedure :: readInput
        procedure :: print
        procedure, private :: command
        procedure :: isRunning
        procedure :: setStopTimes
        procedure :: getNodeVoltage
        procedure :: getTime
        procedure :: updateNodeCurrent

    end type circuit_t

contains

    subroutine init(this, netlist)
        class(circuit_t) :: this
        character(len=*), intent(in), optional :: netlist
        integer :: res

        res = ngSpice_Init(c_funloc(SendChar), &
                           c_funloc(SendStat), & 
                           c_funloc(ControlledExit), & 
                           c_funloc(SendData), &
                           c_funloc(SendInitData), &
                           c_funloc(BGThreadRunning), &
                           this%nodes)
        if (present(netlist)) then
            call this%loadNetlist(netlist)
        end if
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

    function isRunning(this) result(res)
        class(circuit_t) :: this
        logical :: res
        res = ngSpice_running()
    end function

    subroutine setStopTimes(this, finalTime, dt)
        class(circuit_t) :: this
        real, intent(in) :: finalTime, dt
        real :: time = 0.0
        character(20) :: charTime
        do while (time < finalTime)
            time = time + dt
            write(charTime, '(E10.2)') time
            call this%command('stop when time = '//charTime)
        end do
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

    subroutine readInput(this, input) 
        class(circuit_t) :: this
        character(*), intent(in) :: input(:)
        type(c_ptr) :: argv_c(size(input))
        integer :: i   

        type string
            character(len=:,kind=c_char), allocatable :: item
        end type string
        type(string), target :: tmp(size(input))

        ! character(len=:),pointer :: input(:)
        integer :: out

        do i = 1, size(input)
            ! This may involve kind conversion.
            tmp(i)%item = trim(input(i)) // c_null_char
            argv_c(i) = c_loc(tmp(i)%item)
        end do
          ! Size of the argv array on the C side indicated by a NULL.
        ! argv_c(size(argv_c)) = c_null_ptr
        

        ! type(c_ptr) :: c_input
        ! out =  ngSpice_Circ(input)
        out =  ngSpice_Circ(argv_c)
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
        write(*,*) trim(string)
        if (index('stderr', string) /= 0) then
            SendChar = 1
        end if
    end function

    integer(c_int) function SendStat(status, id, nodes)
        type(c_ptr), value, intent(in) :: status
        integer(c_int), intent(in), value :: id
        type(nodes_t) :: nodes
        character(len=:), pointer :: f_output
        character(len=:), allocatable :: string

        call c_f_pointer(status, f_output)
        string = f_output(1:index(f_output, c_null_char)-1)
        write(*,*) trim(string)
    end function

    integer(c_int) function ControlledExit(status, unloadDll, exitOnQuit, id, nodes)
        logical(c_bool), intent(in) :: unloadDll, exitOnQuit
        integer(c_int), intent(in), value :: status, id
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
        type(string_t) :: res
        character, pointer :: f_output(:) => null()
        integer :: i
        res%name = ""
        res%length = 0
        call c_f_pointer(cName, f_output,[100])
        do i = 1,100
            if (f_output(i) == c_null_char) exit
            res%name(i:i) = f_output(i)
        enddo
        res%length = i-1

    end function

    subroutine updateNodeCurrent(this, node_name, current)
        class(circuit_t) :: this
        real :: current
        character(20) :: sCurrent
        character(*) :: node_name
        ! integer :: nodeIdx
        write(sCurrent, '(E10.2)') current
        ! write(sIdx, '(I0)') nodeIdx
        ! sIdx = trim(sIdx)
        call this%command("alter @I"//trim(node_name)//"[dc] = "//trim(sCurrent))
    end subroutine

    function getNodeVoltage(this, name) result(res)
        class(circuit_t) :: this
        character(len=*), intent(in) :: name
        real :: res
        res = this%nodes%values(findVoltageIndexByName(this%nodes%tags, name))
    end function

    function getTime(this) result(res)
        class(circuit_t) :: this
        real :: res
        res = this%nodes%values(findIndexByName(this%nodes%tags, "time"))
    end function

    function findIndexByName(tags, name) result(res)
        type(string_t) :: tags(:)
        character(len=*), intent(in) :: name
        integer :: res, i
        res = 0
        do i = 1, size(tags)
            if ( tags(i)%name(1:tags(i)%length) == trim(name)) then 
                res = i
                exit
            end if
        end do
    end function    


    function findVoltageIndexByName(tags, name) result(res)
        type(string_t) :: tags(:)
        character(len=*), intent(in) :: name
        integer :: res, i
        res = 0
        do i = 1, size(tags)
            if ( tags(i)%name(1:tags(i)%length) == 'V('//trim(name)//')') then 
                res = i
                exit
            else if ( tags(i)%name(1:tags(i)%length) == trim(name)) then 
                res = i
                exit
            end if
        end do
    end function    

    integer(c_int) function SendData(data, numberOfStructs, id, nodes)
        type(c_ptr), value, intent(in) :: data
        type(nodes_t) :: nodes
        integer(c_int), value :: numberOfStructs, id

        type(vecValuesAll), pointer :: valuesAll
        type(c_ptr), pointer :: values(:)
        type(vecValuesArray), allocatable :: vecsaPtr(:) ! array of pointers to type(c_ptr)
        integer :: i
        
        call c_f_pointer(data, valuesAll) 
        call c_f_pointer(valuesAll%vecsa, values, [valuesAll%vecCount])
        allocate(vecsaPtr(valuesAll%vecCount))

        if (.not.allocated(nodes%values)) then 
            allocate(nodes%values(valuesAll%vecCount))
            allocate(nodes%indices(valuesAll%vecCount))
            allocate(nodes%tags(valuesAll%vecCount))
        end if  
        
        do i = 1, valuesAll%vecCount
            call c_f_pointer(values(i), vecsaPtr(i)%vecValuesPtr)
            nodes%values(i) = vecsaPtr(i)%vecValuesPtr%cReal
            nodes%tags(i) = getName(vecsaPtr(i)%vecValuesPtr%name)
        end do
        call c_f_pointer(values(valuesAll%vecCount), vecsaPtr(valuesAll%vecCount)%vecValuesPtr)

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

        write(*,*) 'BGThreadRunning'
    end function


end module 