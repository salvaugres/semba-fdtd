integer function test_read_shieldedPair() bind (C) result(err)
   use smbjson
   use testingTools

   implicit none

   character(len=*),parameter :: filename = PATH_TO_TEST_DATA//'cases/shieldedPair.fdtd.json'
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
      expected%matriz%totalX = 2
      expected%matriz%totalY = 2
      expected%matriz%totalZ = 20

      ! Expected grid.
      expected%despl%nX = 2
      expected%despl%nY = 2
      expected%despl%nZ = 20

      allocate(expected%despl%desX(2))
      allocate(expected%despl%desY(2))
      allocate(expected%despl%desZ(2))
      expected%despl%desX = 1.0
      expected%despl%desY = 1.0
      expected%despl%desZ = 0.00540
      expected%despl%mx1 = 0
      expected%despl%mx2 = 1
      expected%despl%my1 = 0
      expected%despl%my2 = 1
      expected%despl%mz1 = 0
      expected%despl%mz2 = 19

      ! Expected boundaries.
      expected%front%tipoFrontera(:) = F_MUR

      ! Expected sources.

      ! Expected probes
      ! sonda
      expected%Sonda%length = 2
      expected%Sonda%length_max = 2
      allocate(expected%Sonda%collection(2))
      
      expected%Sonda%collection(1)%outputrequest = "voltage_at_wire_end"
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
      expected%Sonda%collection(1)%len_cor = 1
      expected%Sonda%collection(1)%cordinates(1)%tag = "voltage_at_wire_end"
      expected%Sonda%collection(1)%cordinates(1)%Xi = 2 ! Coord id as tag.
      expected%Sonda%collection(1)%cordinates(1)%Yi = 0
      expected%Sonda%collection(1)%cordinates(1)%Zi = 0
      expected%Sonda%collection(1)%cordinates(1)%Or = NP_COR_DDP
      
      expected%Sonda%collection(2)%outputrequest = "current_at_wire_end"
      expected%Sonda%collection(2)%type1 = NP_T1_PLAIN
      expected%Sonda%collection(2)%type2 = NP_T2_TIME
      expected%Sonda%collection(2)%filename = ' '
      expected%Sonda%collection(2)%tstart = 0.0
      expected%Sonda%collection(2)%tstop = 0.0
      expected%Sonda%collection(2)%tstep = 0.0
      expected%Sonda%collection(2)%fstart = 0.0
      expected%Sonda%collection(2)%fstop = 0.0
      expected%Sonda%collection(2)%fstep = 0.0
      allocate(expected%Sonda%collection(1)%cordinates(1))
      expected%Sonda%collection(2)%len_cor = 1
      expected%Sonda%collection(2)%cordinates(1)%tag = "current_at_wire_end"
      expected%Sonda%collection(2)%cordinates(1)%Xi = 2 ! Coord id as tag.
      expected%Sonda%collection(2)%cordinates(1)%Yi = 0
      expected%Sonda%collection(2)%cordinates(1)%Zi = 0
      expected%Sonda%collection(2)%cordinates(1)%Or = NP_COR_WIRECURRENT
            
      ! Expected thin wires
      allocate(expected%tWires%tw(1))
      expected%tWires%tw(1)%rad=0.0001
      expected%tWires%tw(1)%res=22.9e-3
      expected%tWires%tw(1)%dispfile = trim(adjustl(" "))
      expected%tWires%tw(1)%dispfile_LeftEnd = trim(adjustl(" "))
      expected%tWires%tw(1)%dispfile_RightEnd = trim(adjustl(" "))
      expected%tWires%tw(1)%n_twc=18
      expected%tWires%tw(1)%n_twc_max=18
      allocate(expected%tWires%tw(1)%twc(18))
      expected%tWires%tw(1)%twc(1:18)%srcfile = 'None'
      expected%tWires%tw(1)%twc(1:18)%srctype = 'None'
      expected%tWires%tw(1)%twc(1:18)%i = 11
      expected%tWires%tw(1)%twc(1:18)%j = 11
      expected%tWires%tw(1)%twc(1:18)%k = [(i, i=7, 16)]
      expected%tWires%tw(1)%twc(1:18)%d = DIR_Z
      expected%tWires%tw(1)%twc(1)%nd  = 4
      expected%tWires%tw(1)%twc(2:17)%nd = NO_TAG
      expected%tWires%tw(1)%twc(18)%nd  = 6
      
      expected%tWires%tw(1)%twc(1:18)%tag = trim(adjustl("3"))   ! The polyline id is used as tag.
      
      expected%tWires%tw(1)%tl = MATERIAL_CONS
      expected%tWires%tw(1)%R_LeftEnd = 50
      expected%tWires%tw(1)%tr = MATERIAL_CONS
      expected%tWires%tw(1)%R_RightEnd = 50
      
      expected%tWires%n_tw = 1
      expected%tWires%n_tw_max = 1

      ! Expected mtln type
      
   end function
end function

