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
        procedure :: setStopTimes
        procedure :: getNodeVoltage
        procedure :: updateNodes
        procedure :: getTime
        procedure :: updateNodeCurrent

    end type circuit_t

contains

    subroutine init(this, names, netlist)
        class(circuit_t) :: this
        type(string_t), intent(in), dimension(:), optional :: names
        character(len=*), intent(in), optional :: netlist
        integer :: i

        call start()
        if (present(netlist)) then
            call this%loadNetlist(netlist)
        end if

        if (.not.present(names)) then 
            error stop 'Missing node names'
        end if

        allocate(this%nodes%tags(size(names)))
        allocate(this%nodes%values(size(names)))
        allocate(this%nodes%indices(size(names)))
        do i = 1, size(names)
            this%nodes%tags(i) = names(i)
        end do

    end subroutine

    subroutine loadNetlist(this, netlist)
        class(circuit_t) :: this
        character(len=*, kind=c_char), intent(in) :: netlist
        call command(c_char_'source ' // trim(netlist) // c_null_char)
    end subroutine

    subroutine step(this)
        class(circuit_t) :: this
        if (this%time == 0) then
            call this%run()
        else
            call this%resume()
        end if
        call this%updateNodes()

    end subroutine

    subroutine run(this)
        class(circuit_t) :: this
        call command('run ' // c_null_char)
    end subroutine

    subroutine setStopTimes(this, finalTime, dt)
        class(circuit_t) :: this
        real, intent(in) :: finalTime, dt
        real :: time = 0.0
        character(20) :: charTime
        do while (time < finalTime)
            time = time + dt
            write(charTime, '(E10.2)') time
            call command('stop when time = '//charTime // c_null_char)
        end do
    end subroutine

    subroutine resume(this)
        class(circuit_t) :: this
        call command('resume ' // c_null_char)
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

        do i = 1, size(input)
            tmp(i)%item = trim(input(i)) // c_null_char
            argv_c(i) = c_loc(tmp(i)%item)
        end do
        call circ(argv_c)
    end subroutine

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
        write(sCurrent, '(E10.2)') current
        call command("alter @I"//trim(node_name)//"[dc] = "//trim(sCurrent) // c_null_char)
    end subroutine

    subroutine updateNodes(this) 
        class(circuit_t) :: this
        integer :: i
        type(vectorInfo), pointer :: info
        real(kind=c_double), pointer :: values(:)

        do i = 1, size(this%nodes%tags)
            call c_f_pointer(get_vector_info(trim(this%nodes%tags(i)%name)//c_null_char), info)
            call c_f_pointer(info%vRealData, values,shape=[info%vLength])
            this%nodes%values(i) = values(ubound(values,1))
        end do
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

end module 