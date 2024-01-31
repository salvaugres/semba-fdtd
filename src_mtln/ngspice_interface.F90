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
            type(*) :: returnPtr
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


end module