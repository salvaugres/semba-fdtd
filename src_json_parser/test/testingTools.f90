module testingTools
   use NFDETypes_extension
   implicit none
contains

   subroutine testEquality(error_cnt, expected, problem)
      integer, intent(inout) :: error_cnt
      type(Parseador), intent(in) :: expected, problem

      if (.not. expected%general == problem%general) &
         call testFails(error_cnt, 'Expected and read "general" do not match')

      if (.not. expected%despl == problem%despl) &
         call testFails(error_cnt, 'Expected and read "grid" do not match')
      if (.not. expected%front == problem%front) &
         call testFails(error_cnt, 'Expected and read "boundary" do not match')

      if (.not. expected%plnSrc == problem%plnSrc) &
         call testFails(error_cnt, 'Expected and read "planewave sources" do not match')

      if (.not. expected%oldSONDA == problem%oldSonda) &
         call testFails(error_cnt, 'Expected and read "old probes" do not match')
      if (.not. expected%sonda == problem%sonda) &
         call testFails(error_cnt, 'Expected and read "new probes" do not match')

   end subroutine

   subroutine testFails(testResult, msg)
      integer, intent(inout) :: testResult
      character(len=*), intent(in) :: msg
      testResult = testResult + 1
      write(*, *)  "FAIL: "// msg
   end subroutine
end module
