
integer function test_read_planewave() result(error_cnt)    
    use smbjson
    use NFDETypes_extension

    implicit none

    character(len=*),parameter :: filename = 'planewave.fdtd.json'
    type(Parseador) :: problem, expected
    logical :: areSame
    error_cnt = 0
    
    expected = expectedProblemDescription()

    ! Reads problem
    problem = readProblemDescription(filename)
    
    ! Checks.
    if (.not. expected%general == problem%general) &
        call testFails(error_cnt, 'Expected and read "general" do not match')    
    if (.not. expected%despl == problem%despl) &
        call testFails(error_cnt, 'Expected and read "grid" do not match')
    if (.not. expected%front == problem%front) &
        call testFails(error_cnt, 'Expected and read "boundary" do not match')
    if (.not. expected%plnSrc == problem%plnSrc) &
        call testFails(error_cnt, 'Expected and read "planewave sources" do not match')
    if (.not. expected%oldSONDA == problem%oldSonda) &
        call testFails(error_cnt, 'Expected and read "planewave sources" do not match')

contains   
    function expectedProblemDescription() result (expected)
        use NFDETypes_extension

        type(Parseador) :: expected
        
        call initializeProblemDescription(expected)

        ! Expected general info.
        expected%general%dt = 1e-12
        expected%general%nmax = 1000

        ! Expected grid.
        expected%despl%nX = 10
        expected%despl%nY = 10
        expected%despl%nZ = 10
        
        allocate(expected%despl%desX(1))
        allocate(expected%despl%desY(1))
        allocate(expected%despl%desZ(1))
        expected%despl%desX = (/0.1/)
        expected%despl%desY = (/0.1/)
        expected%despl%desZ = (/0.1/)

        ! Expected boundaries.
        expected%front%tipoFrontera(:) = F_MUR

        ! Expected sources.
        allocate(expected%plnSrc%collection(1))
        expected%plnSrc%collection(1)%nombre_fichero = "gauss_100MHz.exc"
        expected%plnSrc%collection(1)%atributo = ""
        expected%plnSrc%collection(1)%coor1 = (/2, 2, 2/)
        expected%plnSrc%collection(1)%coor2 = (/9, 9, 9/)
        expected%plnSrc%collection(1)%theta = 0.0
        expected%plnSrc%collection(1)%phi = 0.0
        expected%plnSrc%collection(1)%alpha = 1.5708
        expected%plnSrc%collection(1)%beta = 0.0
        expected%plnSrc%collection(1)%isRC=.false.
        expected%plnSrc%collection(1)%nummodes=1
        expected%plnSrc%collection(1)%INCERTMAX=0.0
        
    end function
end function


! Testing tools.
subroutine testFails(testResult, msg)
    integer, intent(inout) :: testResult
    character(len=*), intent(in) :: msg
    testResult = testResult + 1
    write(*, *)  "FAIL: "// msg
end subroutine
