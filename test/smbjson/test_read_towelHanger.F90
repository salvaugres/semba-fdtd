integer function test_read_towelhanger() bind (C) result(err)
   use smbjson
   use testingTools

   implicit none

   character(len=*),parameter :: filename = PATH_TO_TEST_DATA//'cases/towelHanger.fdtd.json'
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
      expected%matriz%totalX = 200
      expected%matriz%totalY = 200
      expected%matriz%totalZ = 164

      ! Expected grid.
      expected%despl%nX = 200
      expected%despl%nY = 200
      expected%despl%nZ = 164

      allocate(expected%despl%desX(200))
      allocate(expected%despl%desY(200))
      allocate(expected%despl%desZ(164))
      expected%despl%desX = 0.00025
      expected%despl%desY = 0.00025
      expected%despl%desZ = 0.00025
      expected%despl%mx1 = 0
      expected%despl%mx2 = 200
      expected%despl%my1 = 0
      expected%despl%my2 = 200
      expected%despl%mz1 = 0
      expected%despl%mz2 = 164

      ! Expected boundaries.
      expected%front%tipoFrontera(:) = F_PML
      expected%front%propiedadesPML(:)%numCapas = 6
      expected%front%propiedadesPML(:)%orden = 2.0
      expected%front%propiedadesPML(:)%refl = 0.001

      ! Expected sources.
      allocate(expected%plnSrc%collection(0))
      ! expected%plnSrc%collection(1)%nombre_fichero = "gauss.exc"
      ! expected%plnSrc%collection(1)%atributo = ""
      ! expected%plnSrc%collection(1)%coor1 = [1, 1, 1]
      ! expected%plnSrc%collection(1)%coor2 = [18, 18, 20]
      ! expected%plnSrc%collection(1)%theta = 1.5708
      ! expected%plnSrc%collection(1)%phi = 0.0
      ! expected%plnSrc%collection(1)%alpha = 0.0
      ! expected%plnSrc%collection(1)%beta = 0.0
      ! expected%plnSrc%collection(1)%isRC=.false.
      ! expected%plnSrc%collection(1)%nummodes=1
      ! expected%plnSrc%collection(1)%INCERTMAX=0.0
      ! expected%plnSrc%nc = 1
      ! expected%plnSrc%nC_max = 1

      ! Expected probes
      ! sonda
      expected%Sonda%length = 1
      expected%Sonda%length_max = 1
      allocate(expected%Sonda%collection(1))
      expected%Sonda%collection(1)%outputrequest = "wire_end"
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
      expected%Sonda%collection(1)%cordinates(1)%tag = "wire_end"
      expected%Sonda%collection(1)%cordinates(1)%Xi = 4 ! Coord id as tag.
      expected%Sonda%collection(1)%cordinates(1)%Yi = 0
      expected%Sonda%collection(1)%cordinates(1)%Zi = 0
      expected%Sonda%collection(1)%cordinates(1)%Or = NP_COR_WIRECURRENT
      
      
      ! Expected thin wires
      allocate(expected%tWires%tw(1))
      expected%tWires%tw(1)%rad=1.3e-7
      expected%tWires%tw(1)%dispfile = trim(adjustl(" "))
      expected%tWires%tw(1)%dispfile_LeftEnd = trim(adjustl(" "))
      expected%tWires%tw(1)%dispfile_RightEnd = trim(adjustl(" "))
      expected%tWires%tw(1)%n_twc=20
      expected%tWires%tw(1)%n_twc_max=20
      allocate(expected%tWires%tw(1)%twc(20))
      expected%tWires%tw(1)%twc(1)%srcfile = 'gauss.exc'
      expected%tWires%tw(1)%twc(1)%srctype = 'VOLT'
      expected%tWires%tw(1)%twc(2:5)%srcfile = 'None'
      expected%tWires%tw(1)%twc(2:5)%srctype = 'None'
      expected%tWires%tw(1)%twc(1:5)%i = 90
      expected%tWires%tw(1)%twc(1:5)%j = 100
      expected%tWires%tw(1)%twc(1:5)%k = [(i, i=80, 84)]
      expected%tWires%tw(1)%twc(1:5)%d = DIR_Z
      expected%tWires%tw(1)%twc(1:5)%nd = -1

      expected%tWires%tw(1)%twc(6:15)%srcfile = 'None'
      expected%tWires%tw(1)%twc(6:15)%srctype = 'None'
      expected%tWires%tw(1)%twc(6:15)%i = [(i, i=90,99)]
      expected%tWires%tw(1)%twc(6:15)%j = 100
      expected%tWires%tw(1)%twc(6:15)%k = 85
      expected%tWires%tw(1)%twc(6:15)%d = DIR_X
      expected%tWires%tw(1)%twc(6:15)%nd = -1

      expected%tWires%tw(1)%twc(16:20)%srcfile = 'None'
      expected%tWires%tw(1)%twc(16:20)%srctype = 'None'
      expected%tWires%tw(1)%twc(16:20)%i = 100
      expected%tWires%tw(1)%twc(16:20)%j = 100
      expected%tWires%tw(1)%twc(16:20)%k = [(i, i=84, 80, -1)]
      expected%tWires%tw(1)%twc(16:20)%d = DIR_Z
      expected%tWires%tw(1)%twc(16:20)%nd = -1

      expected%tWires%tw(1)%twc(1)%nd  = 1
      expected%tWires%tw(1)%twc(6)%nd  = 2
      expected%tWires%tw(1)%twc(16)%nd  = 3
      expected%tWires%tw(1)%twc(20)%nd = 4
      
      expected%tWires%tw(1)%twc(1:20)%tag = trim(adjustl("2"))   ! The polyline id is used as tag.
      
      expected%tWires%tw(1)%tl = SERIES_CONS
      expected%tWires%tw(1)%R_LeftEnd = 50
      expected%tWires%tw(1)%C_LeftEnd = 1e22
      expected%tWires%tw(1)%tr = MATERIAL_CONS
      expected%tWires%tw(1)%C_RightEnd = 1e22
      
      expected%tWires%n_tw = 1
      expected%tWires%n_tw_max = 1
   end function
end function

