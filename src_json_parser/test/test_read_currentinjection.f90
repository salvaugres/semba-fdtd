integer function test_read_currentinjection() result(err)
   use smbjson
   use testingTools

   implicit none

   character(len=*),parameter :: filename = 'cases/currentinjection.fdtd.json'
   type(Parseador) :: problem, expected
   type(parser_t) :: parser
   logical :: areSame
   err = 0

   expected = expectedProblemDescription()
   parser = parser_t(filename)
   problem = parser%readProblemDescription()
   call expect_eq(err, expected, problem)

contains
   function expectedProblemDescription() result (expected)
      type(Parseador) :: expected

      integer :: i

      call initializeProblemDescription(expected)

      ! Expected general info.
      expected%general%dt = 1e-12
      expected%general%nmax = 1000

      ! Excected media matrix.
      expected%matriz%totalX = 20
      expected%matriz%totalY = 20
      expected%matriz%totalZ = 20

      ! Expected grid.
      expected%despl%nX = 20
      expected%despl%nY = 20
      expected%despl%nZ = 20

      allocate(expected%despl%desX(1))
      allocate(expected%despl%desY(1))
      allocate(expected%despl%desZ(1))
      expected%despl%desX = (/0.1/)
      expected%despl%desY = (/0.1/)
      expected%despl%desZ = (/0.1/)

      ! Expected boundaries.
      expected%front%tipoFrontera(:) = F_MUR

      ! Expected materials.
      expected%Mats%n_Mats = 0
      expected%Mats%n_Mats_max = 0
      allocate(expected%mats%Mats(0))

      ! Expected material regions.
      expected%pecRegs%nVols = 0
      expected%pecRegs%nSurfs = 1
      expected%pecRegs%nLins = 1
      expected%pecRegs%nVols_max = 0
      expected%pecRegs%nSurfs_max = 1
      expected%pecRegs%nLins_max = 1
      allocate(expected%pecRegs%Vols(0))
      allocate(expected%pecRegs%Surfs(1))
      allocate(expected%pecRegs%Lins(1))
      
      ! PEC square
      expected%pecRegs%Surfs(1)%Or = +iEz
      expected%pecRegs%Surfs(1)%Xi = 6
      expected%pecRegs%Surfs(1)%Xe = 15
      expected%pecRegs%Surfs(1)%Yi = 6
      expected%pecRegs%Surfs(1)%Ye = 15
      expected%pecRegs%Surfs(1)%Zi = 11
      expected%pecRegs%Surfs(1)%Ze = 11
      expected%pecRegs%Surfs(1)%tag = ''

      ! Exit line
      expected%pecRegs%Lins(1)%Or = +iEy
      expected%pecRegs%Lins(1)%Xi = 11
      expected%pecRegs%Lins(1)%Xe = 11
      expected%pecRegs%Lins(1)%Yi = 16
      expected%pecRegs%Lins(1)%Ye = 20
      expected%pecRegs%Lins(1)%Zi = 11
      expected%pecRegs%Lins(1)%Ze = 11
      expected%pecRegs%Lins(1)%tag = ''

      ! Expected sources.
      expected%nodSrc%n_nodSrc = 1
      expected%nodSrc%n_nodSrc_max = 1
      expected%nodSrc%n_C1P_max = 0
      expected%nodSrc%n_C2P_max = 1
      allocate(expected%nodSrc%NodalSource(1))
      expected%nodSrc%NodalSource(1)%nombre = "waveform.exc"
      expected%nodSrc%NodalSource(1)%isElec = .false.
      expected%nodSrc%NodalSource(1)%isMagnet = .false.
      expected%nodSrc%NodalSource(1)%isCurrent = .true.
      expected%nodSrc%NodalSource(1)%isField = .false.
      expected%nodSrc%NodalSource(1)%isInitialValue = .false.
      allocate(expected%nodSrc%NodalSource(1)%c1P(0))
      allocate(expected%nodSrc%NodalSource(1)%c2P(1))
      expected%nodSrc%NodalSource(1)%n_C2P = 1
      expected%nodSrc%NodalSource(1)%c2P(1)%Or = iEy
      expected%nodSrc%NodalSource(1)%c2P(1)%Xi = 11
      expected%nodSrc%NodalSource(1)%c2P(1)%Xe = 11
      expected%nodSrc%NodalSource(1)%c2P(1)%Yi =  1
      expected%nodSrc%NodalSource(1)%c2P(1)%Ye =  5
      expected%nodSrc%NodalSource(1)%c2P(1)%Zi = 11
      expected%nodSrc%NodalSource(1)%c2P(1)%Ze = 11
      expected%nodSrc%NodalSource(1)%c2P(1)%tag = ''
      expected%nodSrc%NodalSource(1)%c2P(1)%xc = 0.0
      expected%nodSrc%NodalSource(1)%c2P(1)%yc = 1.0
      expected%nodSrc%NodalSource(1)%c2P(1)%zc = 0.0
      ! Expected probes
      ! oldSonda
      expected%oldSONDA%n_probes = 0
      expected%oldSONDA%n_probes_max = 0
      allocate(expected%oldSONDA%probes(0))
      
      ! sonda
      expected%Sonda%len_cor_max = 0
      expected%Sonda%length = 0
      expected%Sonda%length_max = 0
      allocate(expected%Sonda%collection(0))
      
      ! bloqueprobes
      expected%BloquePrb%n_bp = 2
      expected%BloquePrb%n_bp_max = 2
      allocate(expected%BloquePrb%bp(2))
      expected%BloquePrb%bp(1)%outputrequest = "bulk_current_at_entry"
      expected%BloquePrb%bp(1)%FileNormalize = ''
      expected%BloquePrb%bp(1)%type2 = NP_T2_TIME
      expected%BloquePrb%bp(1)%tstart = 0.0
      expected%BloquePrb%bp(1)%tstop = 0.0
      expected%BloquePrb%bp(1)%tstep = 0.0
      expected%BloquePrb%bp(1)%fstart = 0.0
      expected%BloquePrb%bp(1)%fstop = 0.0
      expected%BloquePrb%bp(1)%fstep = 0.0
      expected%BloquePrb%bp(1)%i1 = 10
      expected%BloquePrb%bp(1)%i2 = 11
      expected%BloquePrb%bp(1)%j1 =  3
      expected%BloquePrb%bp(1)%j2 =  3
      expected%BloquePrb%bp(1)%k1 = 10
      expected%BloquePrb%bp(1)%k2 = 11
      expected%BloquePrb%bp(1)%skip = 1
      expected%BloquePrb%bp(1)%nml = iEy
      expected%BloquePrb%bp(1)%t = BcELECT
      expected%BloquePrb%bp(1)%tag = ""

      expected%BloquePrb%bp(2)%outputrequest = "bulk_current_at_exit"
      expected%BloquePrb%bp(2)%FileNormalize = ' '
      expected%BloquePrb%bp(2)%type2 = NP_T2_TIME
      expected%BloquePrb%bp(2)%tstart = 0.0
      expected%BloquePrb%bp(2)%tstop = 0.0
      expected%BloquePrb%bp(2)%tstep = 0.0
      expected%BloquePrb%bp(2)%fstart = 0.0
      expected%BloquePrb%bp(2)%fstop = 0.0
      expected%BloquePrb%bp(2)%fstep = 0.0
      expected%BloquePrb%bp(2)%i1 = 10
      expected%BloquePrb%bp(2)%i2 = 11
      expected%BloquePrb%bp(2)%j1 = 18
      expected%BloquePrb%bp(2)%j2 = 18
      expected%BloquePrb%bp(2)%k1 = 10
      expected%BloquePrb%bp(2)%k2 = 11
      expected%BloquePrb%bp(2)%skip = 1
      expected%BloquePrb%bp(2)%nml = iEy
      expected%BloquePrb%bp(2)%t = BcELECT
      expected%BloquePrb%bp(2)%tag = ""
   end function
end function

