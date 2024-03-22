integer function test_read_sphere() bind (C) result(err)
   use smbjson
   use testingTools

   implicit none

   character(len=*),parameter :: filename = PATH_TO_TEST_DATA//'cases/sphere.fdtd.json'
   type(Parseador) :: pr, ex
   type(parser_t) :: parser
   logical :: areSame
   err = 0

   ex = expectedProblemDescription()
   parser = parser_t(filename)
   pr = parser%readProblemDescription()
   
   if (.not. ex%general == pr%general) call testFails(err, 'Expected and read "general" do not match')
   if (.not. ex%matriz == pr%matriz)   call testFails(err, 'Expected and read "media matrix" do not match')
   if (.not. ex%despl == pr%despl)     call testFails(err, 'Expected and read "grid" do not match')
   if (.not. ex%front == pr%front)     call testFails(err, 'Expected and read "boundary" do not match')
   if (.not. ex%Mats == pr%Mats)       call testFails(err, 'Expected and read "materials" do not match')
   ! -- specific surfs not included DO NOT use comparison --
   if (.not. ex%nodSrc == pr%nodSrc) call testFails(err, 'Expected and read "nodal sources" do not match')
   if (.not. ex%sonda == pr%sonda)         call testFails(err, 'Expected and read "new probes" do not match')
   if (.not. ex%BloquePrb == pr%BloquePrb) call testFails(err, 'Expected and read "block probes" do not match')
   
   if (err == 0) write(*,*) "Read and expected inputs are equal."   
   
contains
   function expectedProblemDescription() result (ex)
      type(Parseador) :: ex

      call initializeProblemDescription(ex)

      ! Expected general info.
      ex%general%dt = 3.85167e-11
      ex%general%nmax = 100

      ! Excected media matrix.
      ex%matriz%totalX = 80
      ex%matriz%totalY = 80
      ex%matriz%totalZ = 80

      ! Expected grid.
      ex%despl%nX = 80
      ex%despl%nY = 80
      ex%despl%nZ = 80

      allocate(ex%despl%desX(1))
      allocate(ex%despl%desY(1))
      allocate(ex%despl%desZ(1))
      ex%despl%desX = 0.025
      ex%despl%desY = 0.025
      ex%despl%desZ = 0.025
      ex%despl%mx1 = 0
      ex%despl%mx2 = 80
      ex%despl%my1 = 0
      ex%despl%my2 = 80
      ex%despl%mz1 = 0
      ex%despl%mz2 = 80

      ! Expected boundaries.
      ex%front%tipoFrontera(:) = F_PML
      ex%front%propiedadesPML(:)%numCapas = 10
      ex%front%propiedadesPML(:)%orden = 2
      ex%front%propiedadesPML(:)%refl = 0.001

      ! Expected material regions.
      ex%pecRegs%nSurfs = 1
      ex%pecRegs%nSurfs_max = 1
      allocate(ex%pecRegs%Surfs(1))
      ! -- specific surfs not included DO NOT use comparison --

      ! Expected sources.
      allocate(ex%plnSrc%collection(1))
      ex%plnSrc%collection(1)%nombre_fichero = "predefinedExcitation.exc"
      ex%plnSrc%collection(1)%atributo = ""
      ex%plnSrc%collection(1)%coor1 = [0, 0, 0]
      ex%plnSrc%collection(1)%coor2 = [80, 80, 80]
      ex%plnSrc%collection(1)%theta = 1.5707963268
      ex%plnSrc%collection(1)%phi = 0.0
      ex%plnSrc%collection(1)%alpha = 1.5707963268
      ex%plnSrc%collection(1)%beta = 4.7123889804
      ex%plnSrc%collection(1)%isRC=.false.
      ex%plnSrc%collection(1)%nummodes=1
      ex%plnSrc%collection(1)%INCERTMAX=0.0
      ex%plnSrc%nc = 1
      ex%plnSrc%nC_max = 1

      ! Expected probes
      allocate(ex%oldSONDA)
      ex%oldSONDA%n_probes = 1
      ex%oldSONDA%n_probes_max = 1
      allocate(ex%oldSONDA%probes(1))
      ex%oldSonda%probes(1)%n_FarField = 1
      ex%oldSonda%probes(1)%n_FarField_max = 1
      allocate(ex%oldSonda%probes(1)%FarField(1))
      ex%oldSonda%probes(1)%FarField(1)%probe%outputrequest = "Far field_log_"
      ex%oldSonda%probes(1)%FarField(1)%probe%fstart = 1e6
      ex%oldSonda%probes(1)%FarField(1)%probe%fstop = 1e9
      ex%oldSonda%probes(1)%FarField(1)%probe%fstep = 30
      allocate(ex%oldSonda%probes(1)%FarField(1)%probe%i(2))
      allocate(ex%oldSonda%probes(1)%FarField(1)%probe%j(2))
      allocate(ex%oldSonda%probes(1)%FarField(1)%probe%k(2))
      ex%oldSonda%probes(1)%FarField(1)%probe%i = [2, 78]
      ex%oldSonda%probes(1)%FarField(1)%probe%j = [2, 78]
      ex%oldSonda%probes(1)%FarField(1)%probe%k = [2, 78]
      ex%oldSonda%probes(1)%FarField(1)%probe%n_cord = 2
      ex%oldSonda%probes(1)%FarField(1)%probe%n_cord_max = 2
      ex%oldSonda%probes(1)%FarField(1)%probe%thetastart = 0.0
      ex%oldSonda%probes(1)%FarField(1)%probe%thetastop  = 180.0
      ex%oldSonda%probes(1)%FarField(1)%probe%thetastep  = 90.0 
      ex%oldSonda%probes(1)%FarField(1)%probe%phistart   = 0.0
      ex%oldSonda%probes(1)%FarField(1)%probe%phistop    = 360.0
      ex%oldSonda%probes(1)%FarField(1)%probe%phistep    = 90.0   
      
   end function
end function

