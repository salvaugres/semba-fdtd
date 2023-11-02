
integer function test_read_planewave() result(error_cnt)    
    use smbjson
    use NFDETypes_extension

    implicit none

    character(len=*),parameter :: filename = 'planewave.fdtd.json'
    type(Parseador) :: problem, expected
    logical :: areSame
    error_cnt = 0
    
    call expectedProblemDescription(expected)

    ! Reads problem
    problem = readProblemDescription(filename)
    
    ! Checks.
    if (.not. expected%general == problem%general) &
        call testFails(error_cnt, 'Expected and read "general" do not match')    
    if (.not. expected%despl == problem%despl) &
        call testFails(error_cnt, 'Expected and read "grid" do not match')
end function

subroutine expectedProblemDescription(expected)
    use NFDETypes_extension

    type(Parseador), intent(inout) :: expected
    
    call initializeProblemDescription(expected)

    ! Expected "general"
    expected%general%dt = 1e-12
    expected%general%nmax = 1000

    ! Expected "despl"
    expected%despl%nX = 10
    expected%despl%nY = 10
    expected%despl%nZ = 10
    
    allocate(expected%despl%desX(1))
    allocate(expected%despl%desY(1))
    allocate(expected%despl%desZ(1))
    expected%despl%desX = (/0.1/)
    expected%despl%desY = (/0.1/)
    expected%despl%desZ = (/0.1/)

    ! --

end subroutine

! Testing tools.
subroutine testFails(testResult, msg)
    integer, intent(inout) :: testResult
    character(len=*), intent(in) :: msg
    testResult = testResult + 1
    write(*, *)  "FAIL: "// msg
end subroutine
