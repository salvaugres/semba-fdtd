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

    type, bind(c) :: pVectorInfo
        type(c_ptr) :: pVectorInfo_ptr ! type vectorInfo
    end type

    type, bind(c) :: vecInfo
        integer(c_int) :: number
        type(c_ptr) :: vecName
        logical(c_bool) :: is_real
        type(c_ptr) :: pdVec
        type(c_ptr) :: pdVecScale
    end type

    type, bind(c) :: pVecInfo
        type(c_ptr) :: pVecInfo_ptr ! vecInfo
    end type
    
    type, bind(C) :: vecInOfAll
        type(c_ptr) :: name
        type(c_ptr) :: title
        type(c_ptr) :: date
        type(c_ptr) :: type
        integer(c_int) :: vecCount
        type(c_ptr) :: vecs !type pVecInfo(:)
    end type

    type, bind(C) :: pVecInOfAll
        type(c_ptr):: pVecInOfAll_ptr !type vecInOfAll
    end type

    type :: vecValuesArray
        type(vecValues), pointer :: vecValuesPtr
    end type

    type, bind(C) :: vecValues
        type(c_ptr) :: name
        real(c_double) :: cReal
        real(c_double) :: cImag
        logical(c_bool) :: isScale
        logical(c_bool) :: isComplex
    end type

    type, bind(C) :: pVecValues
        type(c_ptr) :: pVecValues_ptr ! vecValues
    end type

    type, bind(C) :: vecValuesAll
        integer(c_int) :: vecCount
        integer(c_int) :: vecIndex
        type(c_ptr) :: vecsa ! type pVecValues(:)
    end type

    type, bind(C) :: pVecValuesAll
        type(c_ptr) :: pVecValuesAll_ptr ! vecValuesAll
    end type


    interface

        integer(c_int) function ngSpice_Init(&
            cbSendChar, cbSendStat, cbControlledExit, cbSendData, &
            cbSendInitData, cbBGThreadRunning, returnPtr) bind(C, name="ngSpice_Init")
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

        type(c_ptr) function ngSpice_CurPlot() bind(C, name="ngSpice_CurPlot")
            import :: c_ptr
        end function

        type(pVectorInfo) function ngGet_Vec_Info(name) bind(C, name="ngGet_Vec_Info")
            import :: pVectorInfo, c_char
            character(kind=c_char), dimension(*), intent(in) :: name
        end function

        type(c_ptr) function ngGet_Vec_Info2(name) bind(C, name="ngGet_Vec_Info")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*), intent(in) :: name
        end function

        type(c_ptr) function ngSpice_AllPlots() bind(C, name="ngSpice_AllPlots")
            import :: c_ptr
        end function

        type(c_ptr) function ngSpice_AllVecs(name) bind(C, name="ngSpice_AllVecs")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*), intent(in) :: name
        end function

    end interface


contains

    integer(c_int) function SendChar(output, id, returnPtr) !bind(C, name="SendChar")
        character(len=*), intent(in) :: output
        ! type(c_ptr), value, intent(in) :: output
        integer(c_int), intent(in), value :: id
        real :: returnPtr
        ! type(c_ptr), value, intent(in) :: returnPtr
        character(len=:), pointer :: f_output
        character(len=:), allocatable :: string
        
        SendChar = 0
        ! call c_f_pointer(output, f_output)
        ! string = f_output(1:index(f_output, c_null_char)-1)
        write(*,*) 'SendChar: ', trim(output)
        ! write(*,*) 'SendChar: ', trim(string)
        if (index('stderror Error:', string) /= 0) then
            SendChar = 1
        end if
    end function

    integer(c_int) function SendStat(status, id, returnPtr) bind(C, name="SendStat")
        type(c_ptr), value, intent(in) :: status
        integer(c_int), intent(in), value :: id
        type(c_ptr), value, intent(in) :: returnPtr
        character(len=:), pointer :: f_output
        character(len=:), allocatable :: string

        call c_f_pointer(status, f_output)
        string = f_output(1:index(f_output, c_null_char)-1)
        write(*,*) 'SendStat: ', trim(string)
    end function

    integer(c_int) function ControlledExit(status, unloadDll, exitOnQuit, id, returnPtr) bind(C, name="ControlledExit")
        logical(c_bool), intent(in) :: unloadDll, exitOnQuit
        integer(c_int), intent(in), value :: status, id
        type(c_ptr), value, intent(in) :: returnPtr
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

    integer(c_int) function SendData(data, numberOfStructs, id, returnPtr) bind(C, name="SendData")
        type(c_ptr), value, intent(in) :: data, returnPtr
        integer(c_int), value :: numberOfStructs, id

        type(vecValuesAll), pointer :: valuesAll
        type(c_ptr), pointer :: values(:)
        type(vecValuesArray), allocatable :: vecsaPtr(:) ! array of pointers to type(c_ptr)

        integer :: i

        write(*,*) 'SendData begin'

        call c_f_pointer(data, valuesAll) 
        call c_f_pointer(valuesAll%vecsa, values, [valuesAll%vecCount])
        allocate(vecsaPtr(valuesAll%vecCount))
        do i = 1, valuesAll%vecCount
            call c_f_pointer(values(i), vecsaPtr(i)%vecValuesPtr)
            write(*,*) trim(getName(vecsaPtr(i)%vecValuesPtr%name)), vecsaPtr(i)%vecValuesPtr%cReal
        end do
        ! how to make this information available to circuit_t?
        ! how to make it be accesible through the calls to get_vec_info?
        write(*,*) 'SendData end'

    end function

    integer(c_int) function SendInitData(initData, id, returnPtr) bind(C, name="SendInitData")
        integer(c_int), value :: id
        type(vecInOfAll), pointer, intent(in) :: initData
        ! type(pVecInOfAll), value, intent(in) :: initData
        ! type(c_ptr), value, intent(in) :: initData
        type(c_ptr), value, intent(in) :: returnPtr
        ! type(vecInOfAll), pointer :: values
        ! structsPtr%vecCount
        ! call c_f_pointer(initData, values) 
        write(*,*) 'SendInitData'


    end function

    integer(c_int) function BGThreadRunning(isBGThreadNotRunning, id, returnPtr) bind(C, name="BGThreadRunning")
        logical(c_bool) :: isBGThreadNotRunning
        integer(c_int), value :: id
        type(c_ptr), value, intent(in) :: returnPtr
        ! write(*,*) 'isBGThreadNotRunning: ', isBGThreadNotRunning, '. id: ', id
    end function


end module