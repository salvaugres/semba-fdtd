module testingTools
   use NFDETypes_extension
   implicit none
contains

   subroutine testEquality(error_cnt, ex, pr)
      integer, intent(inout) :: error_cnt
      type(Parseador), intent(in) :: ex, pr

      ! Basics
      if (.not. ex%general == pr%general) call testFails(error_cnt, 'Expected and read "general" do not match')
      if (.not. ex%matriz == pr%matriz)   call testFails(error_cnt, 'Expected and read "media matrix" do not match')
      if (.not. ex%despl == pr%despl)     call testFails(error_cnt, 'Expected and read "grid" do not match')
      if (.not. ex%front == pr%front)     call testFails(error_cnt, 'Expected and read "boundary" do not match')
      ! Materials
      if (.not. ex%Mats == pr%Mats)       call testFails(error_cnt, 'Expected and read "materials" do not match')
      if (.not. ex%pecRegs == pr%pecRegs) call testFails(error_cnt, 'Expected and read "pec regions" do not match')
      if (.not. ex%pmcRegs == pr%pmcRegs) call testFails(error_cnt, 'Expected and read "pmc regions" do not match')
      ! if (.not. ex%DielRegs == pr%DielRegs)             &
      !    call testFails(error_cnt, 'Expected and read "dielectric regions" do not match')
      ! if (.not. ex%LossyThinSurfs == pr%LossyThinSurfs) &
      !    call testFails(error_cnt, 'Expected and read "lossy thin surfs" do not match')
      ! if (.not. ex%frqDepMats == pr%frqDepMats)         &
      !    call testFails(error_cnt, 'Expected and read "frq. dep. materials" do not match')
      ! if (.not. ex%aniMats == pr%aniMats)               &
      !    call testFails(error_cnt, 'Expected and read "anisotropic materials" do not match')
      ! Sources
      ! if (.not. ex%boxSrc == pr%boxSrc) call testFails(error_cnt, 'Expected and read "box sources" do not match')
      if (.not. ex%plnSrc == pr%plnSrc) call testFails(error_cnt, 'Expected and read "planewave sources" do not match')
      ! if (.not. ex%nodSrc == pr%nodSrc) call testFails(error_cnt, 'Expected and read "nodal sources" do not match')
      ! Probes
      if (.not. ex%oldSONDA == pr%oldSonda)   call testFails(error_cnt, 'Expected and read "old probes" do not match')
      if (.not. ex%sonda == pr%sonda)         call testFails(error_cnt, 'Expected and read "new probes" do not match')
      ! if (.not. ex%BloquePrb == pr%BloquePrb) call testFails(error_cnt, 'Expected and read "block probes" do not match')
      ! if (.not. ex%VolPrb == pr%VolPrb)       call testFails(error_cnt, 'Expected and read "vol probes" do not match')
      ! Thin elements
      if (.not. ex%tWires == pr%tWires) call testFails(error_cnt, 'Expected and read "thin wires" do not match')
      ! if (.not. ex%sWires == pr%sWires) call testFails(error_cnt, 'Expected and read "slanted wires" do not match')
      ! if (.not. ex%tSlots == pr%tSlots) call testFails(error_cnt, 'Expected and read "thin slots" do not match')
   end subroutine

   subroutine testFails(testResult, msg)
      integer, intent(inout) :: testResult
      character(len=*), intent(in) :: msg
      testResult = testResult + 1
      write(*, *)  "FAIL: "// msg
   end subroutine
end module
