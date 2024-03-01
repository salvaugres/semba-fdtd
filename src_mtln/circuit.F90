module circuit_mod

    use ngspice_interface_mod
    implicit none

    type string_t
        character(len=100) :: name
        integer :: length
    end type string_t

    type source_t
        logical :: has_source = .false.
        real, dimension(:), allocatable :: time
        real, dimension(:), allocatable :: voltage
    contains 
        procedure :: interpolate
    end type

    type VI_t
        real :: v1,v2,i1,i2
        real :: v_th
        real :: r_th
        real :: voltage
        real :: current
        real :: time
    end type

    type nodes_t
        ! real :: time
        type(VI_T), allocatable :: values(:)
        ! real, allocatable :: currents(:)
        ! real, allocatable :: r_eq(:)
        type(source_t), allocatable :: sources(:)
        type(string_t), allocatable :: names(:)
    end type nodes_t

    type, public :: circuit_t
        character (len=:), allocatable :: name
        real :: time = 0.0, dt = 0.0
        logical :: errorFlag = .false.
        type(nodes_t) :: nodes, saved_nodes   

    contains
        procedure :: init
        procedure :: run
        procedure :: step
        procedure, private :: resume
        procedure :: quit
        procedure, private :: loadNetlist
        procedure :: readInput
        procedure :: setStopTimes
        procedure :: getNodeVoltage
        procedure :: getNodeCurrent
        ! procedure :: getNodeREq
        procedure :: updateNodes
        procedure :: saveCircuitState
        procedure :: loadCircuitState
        procedure :: getTime
        procedure :: updateNodeCurrent
        procedure :: updateNodeVoltage
        procedure :: updateVoltageSources
        procedure :: zeroVoltageSources
        procedure :: computeTheveninEquivalent
        procedure, private :: computeTheveninR
        procedure, private :: computeTheveninV
        procedure, private :: updateTheveninR
        procedure, private :: updateTheveninV

        procedure :: getNodeV1
        procedure :: getNodeV2
        procedure :: getNodeI1
        procedure :: getNodeI2
        procedure :: updateNodesVI1
        procedure :: updateNodesVI2
        procedure, private :: updateTheveninR12

        procedure :: getNodeTheveninR
        procedure :: getNodeTheveninV

    end type circuit_t

contains

    real function interpolate(this, time, dt) result(res)
        class(source_t) :: this
        real :: time, dt
        integer :: index
        real, dimension(:), allocatable :: timediff
        ! timediff = this%time - time - 0.5*dt
        ! if (time == 0.0) then 
        !     res = 0.0
        !     return
        ! end if
        timediff = this%time - time
        index = maxloc(timediff, 1, (timediff) <= 0)
        res = 0.5*(this%voltage(index+1) + this%voltage(index))
    end function

    subroutine init(this, names, sources, netlist)
        class(circuit_t) :: this
        type(string_t), intent(in), dimension(:), optional :: names
        type(string_t), intent(in), dimension(:), optional :: sources
        character(len=*), intent(in), optional :: netlist
        integer :: i

        call start()
        if (present(netlist)) then
            call this%loadNetlist(netlist)
        end if

        if (.not.present(names)) then 
            error stop 'Missing node names'
        end if

        allocate(this%nodes%names(size(names)))
        allocate(this%nodes%values(size(names)))
        ! allocate(this%nodes%r_eq(size(names)))
        allocate(this%nodes%sources(size(names)))
        do i = 1, size(names)
            this%nodes%names(i) = names(i)
        end do
        if (present(sources)) then 
            do i = 1, size(sources)
                this%nodes%sources(i) = setSource(sources(i))
            end do
        end if

    end subroutine

    type(source_t) function setSource(source_path) result(res)
        type(string_t), intent(in) :: source_path
        real :: time, v
        integer :: io
        allocate(res%time(0), res%voltage(0))
        if (source_path%length /= 0 ) then 
            res%has_source = .true.
            open(unit = 1, file = source_path%name)
            do
                read(1, *, iostat = io) time, v
                if (io /= 0) exit
                res%time = [res%time, time]
                res%voltage = [res%voltage, v]
            end do
        end if
    end function    

    subroutine loadNetlist(this, netlist)
        class(circuit_t) :: this
        character(len=*, kind=c_char), intent(in) :: netlist
        call command(c_char_'source ' // trim(netlist) // c_null_char)
    end subroutine

    subroutine step(this)
        class(circuit_t) :: this
        call this%updateVoltageSources(this%time) ! done when thevenin
        if (this%time == 0) then
            call this%run()
        else
            ! call this%loadCircuitState()
            call this%resume()
        end if
        call this%updateNodes()
        ! call this%saveCircuitState()

    end subroutine

    subroutine run(this)
        class(circuit_t) :: this
        call command('run ' // c_null_char)
    end subroutine

    subroutine loadCircuitState(this)
        class (circuit_t) :: this
        integer :: i
        character(20) :: v
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then 
                write(v, *) this%saved_nodes%values(i)%voltage
                call command('set v('//this%nodes%names(i)%name//') = '//v// c_null_char)
            end if
        end do
        ! call this%command(' status.txt'// c_null_char)
    end subroutine

    subroutine saveCircuitState(this)
        class (circuit_t) :: this
        integer :: i
        type(vectorInfo), pointer :: info, info_I, info_V
        real(kind=c_double), pointer :: values(:), values_I(:), values_V(:)
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then 
                call c_f_pointer(get_vector_info(trim(this%nodes%names(i)%name)//c_null_char), info_V)
                call c_f_pointer(info_V%vRealData, values_V,shape=[info_V%vLength])
                this%saved_nodes%values(i)%voltage = values_V(ubound(values_V,1))
            end if
        end do
        ! call this%command('wrnodev status.txt'// c_null_char)
    end subroutine

    subroutine setStopTimes(this, finalTime, dt)
        class(circuit_t) :: this
        real, intent(in) :: finalTime, dt
        character(20) :: charTime
        real :: time

        ! time = 0.0
        time = 0.5*dt
        do while (time < finalTime)
            time = time + dt
            write(charTime, '(E10.4)') time
            call command('stop when time = '//charTime // c_null_char)
            ! write(charTime, '(E10.4)') time + 0.1*dt
            ! call command('stop when time = '//charTime // c_null_char)
            ! write(charTime, '(E10.4)') time + 0.1*dt
            ! call command('stop when time = '//charTime // c_null_char)
        end do
    end subroutine

    subroutine resume(this)
        class(circuit_t) :: this
        call command('resume ' // c_null_char)
    end subroutine

    subroutine quit(this)
        class(circuit_t) :: this
        call command('quit 0' // c_null_char)
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

    subroutine updateVoltageSources(this, time)
        class(circuit_t) :: this
        real, intent(in) :: time
        real :: interp
        character(20) :: sVoltage
        integer :: i, index
        do i = 1, size(this%nodes%sources)
            if (this%nodes%sources(i)%has_source) then
                interp = this%nodes%sources(i)%interpolate(time, this%dt) 
                write(*,*) 'source: ', interp
                write(sVoltage, '(E10.4)') interp
                call command("alter @V"//trim(this%nodes%names(i)%name)//"[dc] = "//trim(sVoltage) // c_null_char)
            end if
        end do
    end subroutine

    subroutine zeroVoltageSources(this)
        class(circuit_t) :: this
        character(20) :: sVoltage
        integer :: i
        do i = 1, size(this%nodes%sources)
            if (this%nodes%sources(i)%has_source) then
                write(sVoltage, '(E10.4)') 0.0
                call command("alter @V"//trim(this%nodes%names(i)%name)//"[dc] = "//trim(sVoltage) // c_null_char)
            end if
        end do
    end subroutine

    subroutine updateNodeCurrent(this, node_name, current)
        class(circuit_t) :: this
        real :: current
        character(20) :: sCurrent
        character(*) :: node_name
        if (index(node_name, "initial") /= 0) then
            write(sCurrent, '(E10.4)') current
        else if (index(node_name, "end") /= 0) then
            write(sCurrent, '(E10.4)') -1.0*current
        end if
        call command("alter @I"//trim(node_name)//"[dc] = "//trim(sCurrent) // c_null_char)
    end subroutine

    subroutine updateNodeVoltage(this, node_name, voltage)
        class(circuit_t) :: this
        real, intent(in) :: voltage
        character(20) :: sVoltage
        character(*) :: node_name
        if (index(node_name, "initial") /= 0) then
            write(sVoltage, '(E10.4)') voltage
            call command("alter @V1"//trim(node_name)//"[dc] = "//trim(sVoltage) // c_null_char)
        else
            write(sVoltage, '(E10.4)') voltage
            write(sCurrent, '(E10.4)') current
            call command("alter @V1"//trim(node_name)//"[dc] = "//trim(sVoltage) // c_null_char)
        end if
    end subroutine

    subroutine updateNodesVI2(this) 
        class(circuit_t) :: this
        integer :: i
        type(vectorInfo), pointer :: info, info_I, info_V
        real(kind=c_double), pointer :: values(:), values_I(:), values_V(:)
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then 
                call c_f_pointer(get_vector_info(trim(this%nodes%names(i)%name)//c_null_char), info_V)
                call c_f_pointer(info_V%vRealData, values_V,shape=[info_V%vLength])
                this%nodes%values(i)%v2 = values_V(ubound(values_V,1))
                call c_f_pointer(get_vector_info("V1"//trim(this%nodes%names(i)%name)//"#branch"//c_null_char), info_I)
                call c_f_pointer(info_I%vRealData, values_I,shape=[info_I%vLength])
                this%nodes%values(i)%i2 = values_I(ubound(values_I,1))

            else 
                call c_f_pointer(get_vector_info(trim(this%nodes%names(i)%name)//c_null_char), info)
                call c_f_pointer(info%vRealData, values,shape=[info%vLength])
                this%nodes%values(i)%time = values(ubound(values,1))
            end if
        end do
    end subroutine

    subroutine updateNodesVI1(this) 
        class(circuit_t) :: this
        integer :: i
        type(vectorInfo), pointer :: info, info_I, info_V
        real(kind=c_double), pointer :: values(:), values_I(:), values_V(:)
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then 
                call c_f_pointer(get_vector_info(trim(this%nodes%names(i)%name)//c_null_char), info_V)
                call c_f_pointer(info_V%vRealData, values_V,shape=[info_V%vLength])
                this%nodes%values(i)%v1 = values_V(ubound(values_V,1))
                call c_f_pointer(get_vector_info("V1"//trim(this%nodes%names(i)%name)//"#branch"//c_null_char), info_I)
                call c_f_pointer(info_I%vRealData, values_I,shape=[info_I%vLength])
                this%nodes%values(i)%i1 = values_I(ubound(values_I,1))

            else 
                call c_f_pointer(get_vector_info(trim(this%nodes%names(i)%name)//c_null_char), info)
                call c_f_pointer(info%vRealData, values,shape=[info%vLength])
                this%nodes%values(i)%time = values(ubound(values,1))
            end if
        end do
    end subroutine

    subroutine updateNodes(this) 
        class(circuit_t) :: this
        integer :: i
        type(vectorInfo), pointer :: info, info_I, info_V
        real(kind=c_double), pointer :: values(:), values_I(:), values_V(:)
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then 
                call c_f_pointer(get_vector_info(trim(this%nodes%names(i)%name)//c_null_char), info_V)
                call c_f_pointer(info_V%vRealData, values_V,shape=[info_V%vLength])
                this%nodes%values(i)%voltage = values_V(ubound(values_V,1))
                ! call c_f_pointer(get_vector_info("V1"//trim(this%nodes%names(i)%name)//"#branch"//c_null_char), info_I)
                ! call c_f_pointer(info_I%vRealData, values_I,shape=[info_I%vLength])
                ! this%nodes%values(i)%current = values_I(ubound(values_I,1))

            else 
                call c_f_pointer(get_vector_info(trim(this%nodes%names(i)%name)//c_null_char), info)
                call c_f_pointer(info%vRealData, values,shape=[info%vLength])
                this%nodes%values(i)%time = values(ubound(values,1))
            end if
        end do
    end subroutine

    ! subroutine computeTheveninEquivalent(this)
    !     class(circuit_t) :: this
    !     call this%computeTheveninR()
    !     call this%computeTheveninV()
    ! end subroutine

    subroutine computeTheveninEquivalent(this)
        class(circuit_t) :: this
        integer :: i
        real :: v, kp, km, f, v_0
        character(20) :: s_v
        
        f = 0.1
        kp = 1.0+f
        km = 1.0-f
        v_0 = 1.0

        call this%updateVoltageSources(this%time)

        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then
                ! v = this%getNodeVoltage(trim(this%nodes%names(i)%name))
                ! write(s_v, '(E10.4)') v*kp
                write(s_v, '(E10.4)') v_0*kp
                call command("alter @V1"//trim(this%nodes%names(i)%name)//"[dc] = "//trim(s_v) // c_null_char)
            end if
        end do
        call this%resume()
        call this%updateNodesVI1()

        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then
                ! v = this%getNodeVoltage(trim(this%nodes%names(i)%name))
                ! write(s_v, '(E10.4)') v*km/kp
                write(s_v, '(E10.4)') v_0*km
                call command("alter @V1"//trim(this%nodes%names(i)%name)//"[dc] = "//trim(s_v) // c_null_char)
            end if
        end do
        call this%resume()
        call this%updateNodesVI2()
        
        call this%updateTheveninR12()

    end subroutine

    subroutine updateTheveninR12(this) 
        class(circuit_t) :: this
        integer :: i
        real :: v1,v2,i1,i2
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then 
                v1 = this%getNodeV1(this%nodes%names(i)%name)
                v2 = this%getNodeV2(this%nodes%names(i)%name)
                i1 = this%getNodeI1(this%nodes%names(i)%name)
                i2 = this%getNodeI2(this%nodes%names(i)%name)
                this%nodes%values(i)%r_th = abs((v1-v2)/(i1-i2))
                this%nodes%values(i)%v_th = ((i2*v1-i1*v2)/(i2-i1))
            end if
        end do
    end subroutine


    subroutine computeTheveninR(this)
        class(circuit_t) :: this
        integer :: i
        character(20) :: s_r_short, s_v_test

        ! set voltage sources in circuit to 0
        call this%zeroVoltageSources()
        write(s_r_short, '(E10.4)') 0.0
        ! change open to short
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then
                call command("alter @Ropen"//trim(this%nodes%names(i)%name)//" = "//trim(s_r_short) // c_null_char)
            end if
        end do
        ! set test sources with 1V
        ! write(s_v_test, '(E10.4)') 0.0
        write(s_v_test, '(E10.4)') 1.0
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then
                ! call command("alter @V1"//trim(node_name)//"[dc] = "//trim(sVoltage) // c_null_char)

                call command("alter @V1"//trim(this%nodes%names(i)%name)//"[dc] = "//trim(s_v_test) // c_null_char)
            end if
        end do
        call this%resume()
        call this%updateTheveninR()
    end subroutine

    subroutine updateTheveninR(this) 
        class(circuit_t) :: this
        integer :: i
        type(vectorInfo), pointer :: info, info_I, info_V
        real(kind=c_double), pointer :: values(:), values_I(:), values_V(:)
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then 
                call c_f_pointer(get_vector_info(trim(this%nodes%names(i)%name)//c_null_char), info_V)
                call c_f_pointer(info_V%vRealData, values_V,shape=[info_V%vLength])
                ! this%nodes%values(i)%voltage = values_V(ubound(values_V,1))
                call c_f_pointer(get_vector_info("V1"//trim(this%nodes%names(i)%name)//"#branch"//c_null_char), info_I)
                call c_f_pointer(info_I%vRealData, values_I,shape=[info_I%vLength])
                this%nodes%values(i)%current = values_I(ubound(values_I,1))

                ! this%nodes%values(i)%r_th = abs(this%nodes%values(i)%voltage/this%nodes%values(i)%current)

            else 
                call c_f_pointer(get_vector_info(trim(this%nodes%names(i)%name)//c_null_char), info)
                call c_f_pointer(info%vRealData, values,shape=[info%vLength])
                this%nodes%values(i)%time = values(ubound(values,1))
            end if
        end do
    end subroutine

    subroutine computeTheveninV(this)
        class(circuit_t) :: this
        integer :: i
        character(20) :: s_r_short, s_voltage

        ! set voltage sources in circuit to their value at time
        call this%updateVoltageSources(this%time)
        write(s_r_short, '(E10.4)') 1e20
        ! change short to open
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then
                call command("alter @Ropen"//trim(this%nodes%names(i)%name)//" = "//trim(s_r_short) // c_null_char)
            end if
        end do
        call this%resume()
        call this%updateTheveninV()
        ! change open to short
        write(s_r_short, '(E10.4)') 0.0
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then
                call command("alter @Ropen"//trim(this%nodes%names(i)%name)//" = "//trim(s_r_short) // c_null_char)
            end if
        end do
    end subroutine

    subroutine updateTheveninV(this) 
        class(circuit_t) :: this
        integer :: i
        type(vectorInfo), pointer :: info, info_V
        real(kind=c_double), pointer :: values(:), values_V(:)
        do i = 1, size(this%nodes%names)
            if (this%nodes%names(i)%name /= "time") then 
                call c_f_pointer(get_vector_info(trim(this%nodes%names(i)%name)//c_null_char), info_V)
                call c_f_pointer(info_V%vRealData, values_V,shape=[info_V%vLength])
                this%nodes%values(i)%v_th = values_V(ubound(values_V,1))
                this%nodes%values(i)%r_th = values_V(ubound(values_V,1))/this%nodes%values(i)%current
            else 
                call c_f_pointer(get_vector_info(trim(this%nodes%names(i)%name)//c_null_char), info)
                call c_f_pointer(info%vRealData, values,shape=[info%vLength])
                this%nodes%values(i)%time = values(ubound(values,1))
            end if
        end do
    end subroutine


    function getNodeTheveninR(this, name) result(res)
        class(circuit_t) :: this
        character(len=*), intent(in) :: name
        real :: res
        res = this%nodes%values(findVoltageIndexByName(this%nodes%names, name))%r_th
    end function

    function getNodeTheveninV(this, name) result(res)
        class(circuit_t) :: this
        character(len=*), intent(in) :: name
        real :: res
        res = this%nodes%values(findVoltageIndexByName(this%nodes%names, name))%v_th
    end function

    function getNodeVoltage(this, name) result(res)
        class(circuit_t) :: this
        character(len=*), intent(in) :: name
        real :: res
        res = this%nodes%values(findVoltageIndexByName(this%nodes%names, name))%voltage
    end function

    function getNodeV1(this, name) result(res)
        class(circuit_t) :: this
        character(len=*), intent(in) :: name
        real :: res
        res = this%nodes%values(findVoltageIndexByName(this%nodes%names, name))%v1
    end function
    function getNodeV2(this, name) result(res)
        class(circuit_t) :: this
        character(len=*), intent(in) :: name
        real :: res
        res = this%nodes%values(findVoltageIndexByName(this%nodes%names, name))%v2
    end function

    function getNodeI1(this, name) result(res)
        class(circuit_t) :: this
        character(len=*), intent(in) :: name
        real :: res
        res = this%nodes%values(findVoltageIndexByName(this%nodes%names, name))%i1
    end function

    function getNodeI2(this, name) result(res)
        class(circuit_t) :: this
        character(len=*), intent(in) :: name
        real :: res
        res = this%nodes%values(findVoltageIndexByName(this%nodes%names, name))%i2
    end function

    function getNodeCurrent(this, name) result(res)
        class(circuit_t) :: this
        character(len=*), intent(in) :: name
        real :: res
        res = this%nodes%values(findVoltageIndexByName(this%nodes%names, name))%current
    end function

    function getTime(this) result(res)
        class(circuit_t) :: this
        real :: res
        res = this%nodes%values(findIndexByName(this%nodes%names, "time"))%time
    end function

    function findIndexByName(names, name) result(res)
        type(string_t) :: names(:)
        character(len=*), intent(in) :: name
        integer :: res, i
        res = 0
        do i = 1, size(names)
            if ( names(i)%name(1:names(i)%length) == trim(name)) then 
                res = i
                exit
            end if
        end do
    end function    

    function findVoltageIndexByName(names, name) result(res)
        type(string_t) :: names(:)
        character(len=*), intent(in) :: name
        integer :: res, i
        res = 0
        do i = 1, size(names)
            if ( names(i)%name(1:names(i)%length) == 'V('//trim(name)//')') then 
                res = i
                exit
            else if ( names(i)%name(1:names(i)%length) == trim(name)) then 
                res = i
                exit
            end if
        end do
    end function    

end module 