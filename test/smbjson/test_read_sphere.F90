integer function test_read_sphere() bind (C) result(err)
   use smbjson
   use testingTools

   implicit none

   character(len=*),parameter :: filename = PATH_TO_TEST_DATA//'cases/sphere.fdtd.json'
   type(Parseador) :: problem, expected
   type(parser_t) :: parser
   logical :: areSame
   err = 0

   expected = expectedProblemDescription()
   parser = parser_t(filename)
   problem = parser%readProblemDescription()
   
   if (.not. ex%general == pr%general) call testFails(err, 'Expected and read "general" do not match')
   if (.not. ex%matriz == pr%matriz)   call testFails(err, 'Expected and read "media matrix" do not match')
   if (.not. ex%despl == pr%despl)     call testFails(err, 'Expected and read "grid" do not match')
   if (.not. ex%front == pr%front)     call testFails(err, 'Expected and read "boundary" do not match')
   if (.not. ex%Mats == pr%Mats)       call testFails(err, 'Expected and read "materials" do not match')
   if (.not. ex%pecRegs == pr%pecRegs) call testFails(err, 'Expected and read "pec regions" do not match')
   if (.not. ex%nodSrc == pr%nodSrc) call testFails(err, 'Expected and read "nodal sources" do not match')
   if (.not. ex%sonda == pr%sonda)         call testFails(err, 'Expected and read "new probes" do not match')
   if (.not. ex%BloquePrb == pr%BloquePrb) call testFails(err, 'Expected and read "block probes" do not match')
   
   if (err == 0) write(*,*) "Read and expected inputs are equal."   
   
contains
   function expectedProblemDescription() result (expected)
      type(Parseador) :: expected

      call initializeProblemDescription(expected)

      ! Expected general info.
      expected%general%dt = 3.85167e-11
      expected%general%nmax = 100

      ! Excected media matrix.
      expected%matriz%totalX = 80
      expected%matriz%totalY = 80
      expected%matriz%totalZ = 80

      ! Expected grid.
      expected%despl%nX = 80
      expected%despl%nY = 80
      expected%despl%nZ = 80

      allocate(expected%despl%desX(80))
      allocate(expected%despl%desY(80))
      allocate(expected%despl%desZ(80))
      expected%despl%desX = 0.025
      expected%despl%desY = 0.025
      expected%despl%desZ = 0.025
      expected%despl%mx1 = 0
      expected%despl%mx2 = 80
      expected%despl%my1 = 0
      expected%despl%my2 = 80
      expected%despl%mz1 = 0
      expected%despl%mz2 = 80

      ! Expected boundaries.
      expected%front%tipoFrontera(:) = F_PML
      expected%front%propiedadesPML(:)%numCapas = 10
      expected%front%propiedadesPML(:)%orden = 2
      expected%front%propiedadesPML(:)%refl = 0.001

      ! Expected sources.
      allocate(expected%plnSrc%collection(1))
      expected%plnSrc%collection(1)%nombre_fichero = "gauss.exc"
      expected%plnSrc%collection(1)%atributo = ""
      expected%plnSrc%collection(1)%coor1 = [1, 1, 1]
      expected%plnSrc%collection(1)%coor2 = [8, 8, 8]
      expected%plnSrc%collection(1)%theta = 1.5707963268
      expected%plnSrc%collection(1)%phi = 0.0
      expected%plnSrc%collection(1)%alpha = 1.5707963268,
      expected%plnSrc%collection(1)%beta = 4.7123889804
      expected%plnSrc%collection(1)%isRC=.false.
      expected%plnSrc%collection(1)%nummodes=1
      expected%plnSrc%collection(1)%INCERTMAX=0.0
      expected%plnSrc%nc = 1
      expected%plnSrc%nC_max = 1

      ! Expected probes
      ! oldSonda
      expected%oldSONDA%n_probes = 0
      expected%oldSONDA%n_probes_max = 0
      allocate(expected%oldSONDA%probes(0))
      ! sonda
      expected%Sonda%len_cor_max = 0
      expected%Sonda%length = 1
      expected%Sonda%length_max = 1
      allocate(expected%Sonda%collection(1))
      expected%Sonda%collection(1)%outputrequest = "electric_field_point_probe"
      expected%Sonda%collection(1)%type1 = NP_T1_PLAIN
      expected%Sonda%collection(1)%type2 = NP_T2_TIME
      expected%Sonda%collection(1)%filename = ' '
      expected%Sonda%collection(1)%tstart = 0.0
      expected%Sonda%collection(1)%tstop = 0.0
      expected%Sonda%collection(1)%tstep = 0.0
      expected%Sonda%collection(1)%fstart = 0.0
      expected%Sonda%collection(1)%fstop = 0.0
      expected%Sonda%collection(1)%fstep = 0.0
      allocate(expected%Sonda%collection(1)%cordinates(3))
      expected%Sonda%collection(1)%len_cor = 3
      expected%Sonda%collection(1)%cordinates(1:3)%tag = "electric_field_point_probe"
      expected%Sonda%collection(1)%cordinates(1:3)%Xi = 4
      expected%Sonda%collection(1)%cordinates(1:3)%Yi = 4
      expected%Sonda%collection(1)%cordinates(1:3)%Zi = 4
      expected%Sonda%collection(1)%cordinates(1)%Or = NP_COR_EX
      expected%Sonda%collection(1)%cordinates(2)%Or = NP_COR_EY
      expected%Sonda%collection(1)%cordinates(3)%Or = NP_COR_EZ

   end function
end function

