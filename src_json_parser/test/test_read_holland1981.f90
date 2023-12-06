integer function test_read_holland1981() result(error_cnt)
   use smbjson
   use testingTools

   implicit none

   character(len=*),parameter :: filename = 'cases/holland1981.fdtd.json'
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
      expected%matriz%totalZ = 22

      ! Expected grid.
      expected%despl%nX = 20
      expected%despl%nY = 20
      expected%despl%nZ = 22

      allocate(expected%despl%desX(1))
      allocate(expected%despl%desY(1))
      allocate(expected%despl%desZ(1))
      expected%despl%desX = (/0.1/)
      expected%despl%desY = (/0.1/)
      expected%despl%desZ = (/0.1/)

      ! Expected boundaries.
      expected%front%tipoFrontera(:) = F_PML
      expected%front%propiedadesPML(:)%numCapas = 6
      expected%front%propiedadesPML(:)%orden = 2.0
      expected%front%propiedadesPML(:)%refl = 0.001

      ! Expected sources.
      allocate(expected%plnSrc%collection(1))
      expected%plnSrc%collection(1)%nombre_fichero = "waveform.exc"
      expected%plnSrc%collection(1)%atributo = ""
      expected%plnSrc%collection(1)%coor1 = (/2, 2, 2/)
      expected%plnSrc%collection(1)%coor2 = (/19, 19, 21/)
      expected%plnSrc%collection(1)%theta = 1.5708
      expected%plnSrc%collection(1)%phi = 0.0
      expected%plnSrc%collection(1)%alpha = 0.0
      expected%plnSrc%collection(1)%beta = 0.0
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
      expected%Sonda%collection(1)%cordinates(1)%tag = ' '
      expected%Sonda%collection(1)%cordinates(1)%Xi = 2 ! Coord id as tag.
      expected%Sonda%collection(1)%cordinates(1)%Yi = 0
      expected%Sonda%collection(1)%cordinates(1)%Zi = 0
      expected%Sonda%collection(1)%cordinates(1)%Or = NP_COR_WIRECURRENT

      ! Expected thin wires
      allocate(expected%tWires%tw(1))
      expected%tWires%tw(1)%rad=0.02
      expected%tWires%tw(1)%dispfile = trim(adjustl(" "))
      expected%tWires%tw(1)%dispfile_LeftEnd = trim(adjustl(" "))
      expected%tWires%tw(1)%dispfile_RightEnd = trim(adjustl(" "))
      
      expected%tWires%tw(1)%n_twc=10
      expected%tWires%tw(1)%n_twc_max=10
      allocate(expected%tWires%tw(1)%twc(10))
      expected%tWires%tw(1)%twc(1:10)%i = 12
      expected%tWires%tw(1)%twc(1:10)%j = 12
      expected%tWires%tw(1)%twc(1:10)%k = [(i, i=8, 17)]
      expected%tWires%tw(1)%twc(1)%tag = "1" 
      expected%tWires%tw(1)%twc(6)%tag = "2"
      expected%tWires%tw(1)%twc(10)%tag = "3"
      
      expected%tWires%tw(1)%tl = MATERIAL_CONS
      expected%tWires%tw(1)%tr = MATERIAL_CONS
      
      expected%tWires%n_tw = 1
      expected%tWires%n_tw_max = 1
   end function
end function

