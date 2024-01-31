integer function test_spice_read_message() result(error_cnt)    

    use circuit_mod
    use testingTools_mod
    ! use ngspice_interface_mod
    implicit none

    type(circuit_t) :: circuit
    real :: result(4)
    character(50), dimension(:), allocatable :: input
    integer :: i
    allocate(input(0))
    input = [input, "* Multiple dc sources "]
    input = [input, "v1 1 0 dc 24"]
    input = [input, "v2 3 0 dc 15"]
    input = [input, "r1 1 2 10k"]
    input = [input, "r2 2 3 8.1k"]
    input = [input, "r3 2 0 4.7k"]
    input = [input, ".dc v1 24 24 1"]
    input = [input, ".save v(3) v(2) v(1)"]
    input = [input, ".end"]
    input = [input, "NULL"]
    ! write(*,*) input
    error_cnt = 0

    call circuit%init()
    call circuit%readInput(input)
    call circuit%run()

    result = [24.000000000000000, 9.7469741675197206, 15.000000000000000, 24.000000000000000]
    if (size(circuit%nodes%values) /= 4) then 
        error_cnt = error_cnt + 1
    end if

    do i = 1, 4                      
        if (checkNear(circuit%nodes%values(i), result(i), 0.01) .eqv. .false. ) then 
            error_cnt = error_cnt + 1
        end if
    end do

    write(*,*) error_cnt

end function