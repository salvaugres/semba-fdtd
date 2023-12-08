integer function test_read_currentinjection() result(error_cnt)
   use smbjson
   use testingTools

   implicit none

   character(len=*),parameter :: filename = 'cases/currentinjection.fdtd.json'
   type(Parseador) :: problem, expected
   type(parser_t) :: parser
   logical :: areSame
   error_cnt = 0

   expected = expectedProblemDescription()
   parser = parser_t(filename)
   problem = parser%readProblemDescription()
   call testEquality(error_cnt, expected, problem)

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
      expected%n_Mats = 0
      expected%n_Mats_max = 0
      allocate(expected%Mats(0))

      ! Expected material regions.
      expected%pecRegs%nVols = 0
      expected%pecRegs%nSurfs = 1
      expected%pecRegs%nLins = 1
      expected%pecRegs%nVols_max = 0
      expected%pecRegs%nSurfs_max = 1
      expected%pecRegs%nLins_max = 1
      allocate(expected%pecRegs%Vols(0))
      allocate(expected%pecRegs%Surfs(1))
      allocate(expected%pecRegs%Lins(2))
      
      ! PEC square
      expected%pecRegs%Surfs(1)%Xi = 6
      expected%pecRegs%Surfs(1)%Yi = 6
      expected%pecRegs%Surfs(1)%Zi = 11
      expected%pecRegs%Surfs(1)%Xe = 15
      expected%pecRegs%Surfs(1)%Ye = 15
      expected%pecRegs%Surfs(1)%Ze = 11
      
      ! Entry line
      expected%pecRegs%Lins(1)%Xi = 11
      expected%pecRegs%Lins(1)%Yi =  1
      expected%pecRegs%Lins(1)%Zi = 11

      expected%pecRegs%Lins(1)%Xe = 11
      expected%pecRegs%Lins(1)%Ye =  5
      expected%pecRegs%Lins(1)%Ze = 11

      ! Exit line
      expected%pecRegs%Lins(1)%Xi = 11
      expected%pecRegs%Lins(1)%Yi = 16
      expected%pecRegs%Lins(1)%Zi = 11

      expected%pecRegs%Lins(1)%Xe = 11
      expected%pecRegs%Lins(1)%Ye = 20
      expected%pecRegs%Lins(1)%Ze = 11

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
      expected%nodSrc%NodalSource(1)%c2P(1)%Xi = 11
      expected%nodSrc%NodalSource(1)%c2P(1)%Yi =  1
      expected%nodSrc%NodalSource(1)%c2P(1)%Zi = 11
      expected%nodSrc%NodalSource(1)%c2P(1)%Xe = 11
      expected%nodSrc%NodalSource(1)%c2P(1)%Ye =  5
      expected%nodSrc%NodalSource(1)%c2P(1)%Ze = 11
      expected%nodSrc%NodalSource(1)%c2P(1)%Or = iEx

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
      expected%Sonda%collection(1)%outputrequest = "mid_point"
      expected%Sonda%collection(1)%type1 = NP_T1_PLAIN
      expected%Sonda%collection(1)%type2 = NP_T2_TIME
      expected%Sonda%collection(1)%filename = ' '
      expected%Sonda%collection(1)%tstart = 0.0
      expected%Sonda%collection(1)%tstop = 0.0
      expected%Sonda%collection(1)%tstep = 0.0
      expected%Sonda%collection(1)%fstart = 0.0
      expected%Sonda%collection(1)%fstop = 0.0
      expected%Sonda%collection(1)%fstep = 0.0
      allocate(expected%Sonda%collection(1)%cordinates(1))
      expected%Sonda%collection(1)%cordinates(1)%tag = '2'
      expected%Sonda%collection(1)%cordinates(1)%Xi = 2 ! Coord id as tag.
      expected%Sonda%collection(1)%cordinates(1)%Yi = 0
      expected%Sonda%collection(1)%cordinates(1)%Zi = 0
      expected%Sonda%collection(1)%cordinates(1)%Or = NP_COR_WIRECURRENT
   end function
end function

