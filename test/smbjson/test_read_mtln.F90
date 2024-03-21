integer function test_read_shieldedpair() bind (C) result(err)
   use smbjson
   use testingTools

   implicit none

   character(len=*),parameter :: filename = PATH_TO_TEST_DATA//'cases/mtln.fdtd.json'
   type(Parseador) :: problem, expected
   type(parser_t) :: parser
   logical :: areSame
   err = 0

   expected = expectedProblemDescription()
   parser = parser_t(filename)
   problem = parser%readProblemDescription()
   call expect_eq(err, expected, problem)
   call expect_eq_mtln(err, expected, problem)
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
      expected%matriz%totalY = 7
      expected%matriz%totalZ = 100

      ! Expected grid.
      expected%despl%nX = 2
      expected%despl%nY = 7
      expected%despl%nZ = 100

      allocate(expected%despl%desX(2))
      allocate(expected%despl%desY(2))
      allocate(expected%despl%desZ(2))
      expected%despl%desX = 0.1
      expected%despl%desY = 0.1
      expected%despl%desZ = 0.1
      expected%despl%mx1 = 0
      expected%despl%mx2 = 2
      expected%despl%my1 = 0
      expected%despl%my2 = 7
      expected%despl%mz1 = 0
      expected%despl%mz2 = 100

      ! Expected boundaries.
      expected%front%tipoFrontera(:) = F_MUR

      ! Expected sources.

      ! Expected probes
      ! oldSonda
      expected%oldSONDA%n_probes = 0
      expected%oldSONDA%n_probes_max = 0
      allocate(expected%oldSONDA%probes(0))

      ! sonda
      expected%Sonda%length = 7
      expected%Sonda%length_max = 7
      allocate(expected%Sonda%collection(7))
      
      expected%Sonda%collection(1)%outputrequest = "b1_terminal_voltage"
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
      expected%Sonda%collection(1)%cordinates(1)%tag = "b1_terminal_voltage"
      expected%Sonda%collection(1)%cordinates(1)%Xi = 11 ! Coord id as tag.
      expected%Sonda%collection(1)%cordinates(1)%Yi = 0
      expected%Sonda%collection(1)%cordinates(1)%Zi = 0
      expected%Sonda%collection(1)%cordinates(1)%Or = NP_COR_DDP
      
      expected%Sonda%collection(2)%outputrequest = "b1_terminal_current"
      expected%Sonda%collection(2)%type1 = NP_T1_PLAIN
      expected%Sonda%collection(2)%type2 = NP_T2_TIME
      expected%Sonda%collection(2)%filename = ' '
      expected%Sonda%collection(2)%tstart = 0.0
      expected%Sonda%collection(2)%tstop = 0.0
      expected%Sonda%collection(2)%tstep = 0.0
      expected%Sonda%collection(2)%fstart = 0.0
      expected%Sonda%collection(2)%fstop = 0.0
      expected%Sonda%collection(2)%fstep = 0.0
      allocate(expected%Sonda%collection(2)%cordinates(1))
      expected%Sonda%collection(2)%len_cor = 1
      expected%Sonda%collection(2)%cordinates(1)%tag = "b1_terminal_current"
      expected%Sonda%collection(2)%cordinates(1)%Xi = 11 ! Coord id as tag.
      expected%Sonda%collection(2)%cordinates(1)%Yi = 0
      expected%Sonda%collection(2)%cordinates(1)%Zi = 0
      expected%Sonda%collection(2)%cordinates(1)%Or = NP_COR_WIRECURRENT
            
      expected%Sonda%collection(3)%outputrequest = "b1_junction_current"
      expected%Sonda%collection(3)%type1 = NP_T1_PLAIN
      expected%Sonda%collection(3)%type2 = NP_T2_TIME
      expected%Sonda%collection(3)%filename = ' '
      expected%Sonda%collection(3)%tstart = 0.0
      expected%Sonda%collection(3)%tstop = 0.0
      expected%Sonda%collection(3)%tstep = 0.0
      expected%Sonda%collection(3)%fstart = 0.0
      expected%Sonda%collection(3)%fstop = 0.0
      expected%Sonda%collection(3)%fstep = 0.0
      allocate(expected%Sonda%collection(3)%cordinates(1))
      expected%Sonda%collection(3)%len_cor = 1
      expected%Sonda%collection(3)%cordinates(1)%tag = "b1_junction_current"
      expected%Sonda%collection(3)%cordinates(1)%Xi = 15 ! Coord id as tag.
      expected%Sonda%collection(3)%cordinates(1)%Yi = 0
      expected%Sonda%collection(3)%cordinates(1)%Zi = 0
      expected%Sonda%collection(3)%cordinates(1)%Or = NP_COR_WIRECURRENT
            
      expected%Sonda%collection(4)%outputrequest = "b2_junction_current"
      expected%Sonda%collection(4)%type1 = NP_T1_PLAIN
      expected%Sonda%collection(4)%type2 = NP_T2_TIME
      expected%Sonda%collection(4)%filename = ' '
      expected%Sonda%collection(4)%tstart = 0.0
      expected%Sonda%collection(4)%tstop = 0.0
      expected%Sonda%collection(4)%tstep = 0.0
      expected%Sonda%collection(4)%fstart = 0.0
      expected%Sonda%collection(4)%fstop = 0.0
      expected%Sonda%collection(4)%fstep = 0.0
      allocate(expected%Sonda%collection(4)%cordinates(1))
      expected%Sonda%collection(4)%len_cor = 1
      expected%Sonda%collection(4)%cordinates(1)%tag = "b2_junction_current"
      expected%Sonda%collection(4)%cordinates(1)%Xi = 15 ! Coord id as tag.
      expected%Sonda%collection(4)%cordinates(1)%Yi = 0
      expected%Sonda%collection(4)%cordinates(1)%Zi = 0
      expected%Sonda%collection(4)%cordinates(1)%Or = NP_COR_WIRECURRENT
            
      expected%Sonda%collection(5)%outputrequest = "b2_terminal_current"
      expected%Sonda%collection(5)%type1 = NP_T1_PLAIN
      expected%Sonda%collection(5)%type2 = NP_T2_TIME
      expected%Sonda%collection(5)%filename = ' '
      expected%Sonda%collection(5)%tstart = 0.0
      expected%Sonda%collection(5)%tstop = 0.0
      expected%Sonda%collection(5)%tstep = 0.0
      expected%Sonda%collection(5)%fstart = 0.0
      expected%Sonda%collection(5)%fstop = 0.0
      expected%Sonda%collection(5)%fstep = 0.0
      allocate(expected%Sonda%collection(5)%cordinates(1))
      expected%Sonda%collection(5)%len_cor = 1
      expected%Sonda%collection(5)%cordinates(1)%tag = "b2_terminal_current"
      expected%Sonda%collection(5)%cordinates(1)%Xi = 23 ! Coord id as tag.
      expected%Sonda%collection(5)%cordinates(1)%Yi = 0
      expected%Sonda%collection(5)%cordinates(1)%Zi = 0
      expected%Sonda%collection(5)%cordinates(1)%Or = NP_COR_WIRECURRENT
            
      expected%Sonda%collection(6)%outputrequest = "b3_junction_current"
      expected%Sonda%collection(6)%type1 = NP_T1_PLAIN
      expected%Sonda%collection(6)%type2 = NP_T2_TIME
      expected%Sonda%collection(6)%filename = ' '
      expected%Sonda%collection(6)%tstart = 0.0
      expected%Sonda%collection(6)%tstop = 0.0
      expected%Sonda%collection(6)%tstep = 0.0
      expected%Sonda%collection(6)%fstart = 0.0
      expected%Sonda%collection(6)%fstop = 0.0
      expected%Sonda%collection(6)%fstep = 0.0
      allocate(expected%Sonda%collection(6)%cordinates(1))
      expected%Sonda%collection(6)%len_cor = 1
      expected%Sonda%collection(6)%cordinates(1)%tag = "b3_junction_current"
      expected%Sonda%collection(6)%cordinates(1)%Xi = 15 ! Coord id as tag.
      expected%Sonda%collection(6)%cordinates(1)%Yi = 0
      expected%Sonda%collection(6)%cordinates(1)%Zi = 0
      expected%Sonda%collection(6)%cordinates(1)%Or = NP_COR_WIRECURRENT
            
      expected%Sonda%collection(7)%outputrequest = "b3_terminal_current"
      expected%Sonda%collection(7)%type1 = NP_T1_PLAIN
      expected%Sonda%collection(7)%type2 = NP_T2_TIME
      expected%Sonda%collection(7)%filename = ' '
      expected%Sonda%collection(7)%tstart = 0.0
      expected%Sonda%collection(7)%tstop = 0.0
      expected%Sonda%collection(7)%tstep = 0.0
      expected%Sonda%collection(7)%fstart = 0.0
      expected%Sonda%collection(7)%fstop = 0.0
      expected%Sonda%collection(7)%fstep = 0.0
      allocate(expected%Sonda%collection(7)%cordinates(1))
      expected%Sonda%collection(7)%len_cor = 1
      expected%Sonda%collection(7)%cordinates(1)%tag = "b3_terminal_current"
      expected%Sonda%collection(7)%cordinates(1)%Xi = 24 ! Coord id as tag.
      expected%Sonda%collection(7)%cordinates(1)%Yi = 0
      expected%Sonda%collection(7)%cordinates(1)%Zi = 0
      expected%Sonda%collection(7)%cordinates(1)%Or = NP_COR_WIRECURRENT
            
      ! Expected thin wires
      allocate(expected%tWires%tw(3))
      ! wire 1
      expected%tWires%tw(1)%rad=0.0001
      expected%tWires%tw(1)%res=22.9e-3
      expected%tWires%tw(1)%dispfile = trim(adjustl(" "))
      expected%tWires%tw(1)%dispfile_LeftEnd = trim(adjustl(" "))
      expected%tWires%tw(1)%dispfile_RightEnd = trim(adjustl(" "))
      expected%tWires%tw(1)%n_twc=9
      expected%tWires%tw(1)%n_twc_max=9
      allocate(expected%tWires%tw(1)%twc(9))
      expected%tWires%tw(1)%twc(1:9)%srcfile = 'None'
      expected%tWires%tw(1)%twc(1:9)%srctype = 'None'
      expected%tWires%tw(1)%twc(1:9)%i = [(i, i=1, 9)]
      expected%tWires%tw(1)%twc(1:9)%j = 1
      expected%tWires%tw(1)%twc(1:9)%k = 1
      expected%tWires%tw(1)%twc(1:9)%d = DIR_X
      expected%tWires%tw(1)%twc(1)%nd  = 1
      expected%tWires%tw(1)%twc(2:8)%nd = NO_TAG
      expected%tWires%tw(1)%twc(9)%nd  = 2
      
      expected%tWires%tw(1)%twc(1:9)%tag = trim(adjustl("1"))   ! The polyline id is used as tag.
      
      expected%tWires%tw(1)%tl = SERIES_CONS
      expected%tWires%tw(1)%R_LeftEnd = 0.7e-3
      expected%tWires%tw(1)%tr = MATERIAL_CONS
      expected%tWires%tw(1)%R_RightEnd = 0.0

      ! wire 2
      expected%tWires%tw(2)%rad=0.0001
      expected%tWires%tw(2)%res=11.8e-3
      expected%tWires%tw(2)%dispfile = trim(adjustl(" "))
      expected%tWires%tw(2)%dispfile_LeftEnd = trim(adjustl(" "))
      expected%tWires%tw(2)%dispfile_RightEnd = trim(adjustl(" "))
      expected%tWires%tw(2)%n_twc=8
      expected%tWires%tw(2)%n_twc_max=8
      allocate(expected%tWires%tw(1)%twc(8))
      expected%tWires%tw(2)%twc(1:8)%srcfile = 'None'
      expected%tWires%tw(2)%twc(1:8)%srctype = 'None'
      expected%tWires%tw(2)%twc(1:8)%i = [(i, i=1, 8)]
      expected%tWires%tw(2)%twc(1:8)%j = 1
      expected%tWires%tw(2)%twc(1:8)%k = 1
      expected%tWires%tw(2)%twc(1:8)%d = DIR_X
      expected%tWires%tw(2)%twc(1)%nd  = 1
      expected%tWires%tw(2)%twc(2:7)%nd = NO_TAG
      expected%tWires%tw(2)%twc(8)%nd  = 2
      
      expected%tWires%tw(2)%twc(1:8)%tag = trim(adjustl("2"))   ! The polyline id is used as tag.
      
      expected%tWires%tw(2)%tl = MATERIAL_CONS
      expected%tWires%tw(2)%R_LeftEnd = 0.0
      expected%tWires%tw(2)%tr = SERIES_CONS
      expected%tWires%tw(2)%R_RightEnd = 1.0

      ! wire 3
      expected%tWires%tw(3)%rad=0.0001
      expected%tWires%tw(3)%res= 17.3e-3
      expected%tWires%tw(3)%dispfile = trim(adjustl(" "))
      expected%tWires%tw(3)%dispfile_LeftEnd = trim(adjustl(" "))
      expected%tWires%tw(3)%dispfile_RightEnd = trim(adjustl(" "))
      expected%tWires%tw(3)%n_twc=7
      expected%tWires%tw(3)%n_twc_max=7
      allocate(expected%tWires%tw(3)%twc(18))
      expected%tWires%tw(3)%twc(1:7)%srcfile = 'None'
      expected%tWires%tw(3)%twc(1:7)%srctype = 'None'
      expected%tWires%tw(3)%twc(1:7)%i = 1
      expected%tWires%tw(3)%twc(1:7)%j = [(i, i=1, 7)]
      expected%tWires%tw(3)%twc(1:7)%k = 1
      expected%tWires%tw(3)%twc(1:7)%d = DIR_Y
      expected%tWires%tw(3)%twc(1)%nd  = 1
      expected%tWires%tw(3)%twc(2:6)%nd = NO_TAG
      expected%tWires%tw(3)%twc(7)%nd  = 2
      
      expected%tWires%tw(3)%twc(1:7)%tag = trim(adjustl("3"))   ! The polyline id is used as tag.
      
      expected%tWires%tw(3)%tl = MATERIAL_CONS
      expected%tWires%tw(3)%R_LeftEnd = 0.0
      expected%tWires%tw(3)%tr = SERIES_CONS
      expected%tWires%tw(3)%R_RightEnd = 0.7e-3

      expected%tWires%n_tw = 3
      expected%tWires%n_tw_max = 3

      ! Expected mtln type
      !connectors
      ! id = 24
      allocate(expected%mtln%connectors(4))
      expected%mtln%connectors(1)%resistances = [100.0e-3]
      expected%mtln%connectors(1)%transfer_impedance_per_meter%direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
      allocate(expected%mtln%connectors(1)%transfer_impedance_per_meter%poles(0))
      allocate(expected%mtln%connectors(1)%transfer_impedance_per_meter%residues(0))

      ! id = 25
      expected%mtln%connectors(2)%resistances = [19.0]
      expected%mtln%connectors(2)%transfer_impedance_per_meter%direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
      allocate(expected%mtln%connectors(2)%transfer_impedance_per_meter%poles(0))
      allocate(expected%mtln%connectors(2)%transfer_impedance_per_meter%residues(0))

      ! id = 204
      expected%mtln%connectors(3)%resistances = [100.0e-3]
      expected%mtln%connectors(3)%transfer_impedance_per_meter%direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
      expected%mtln%connectors(3)%transfer_impedance_per_meter%resistive_term = 3.33
      expected%mtln%connectors(3)%transfer_impedance_per_meter%inductive_term = 2.6e-9
      allocate(expected%mtln%connectors(3)%transfer_impedance_per_meter%poles(0))
      allocate(expected%mtln%connectors(3)%transfer_impedance_per_meter%residues(0))

      ! id = 205
      expected%mtln%connectors(4)%resistances = [100.0e-3]
      expected%mtln%connectors(4)%transfer_impedance_per_meter%direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
      expected%mtln%connectors(4)%transfer_impedance_per_meter%resistive_term = 609.3
      expected%mtln%connectors(4)%transfer_impedance_per_meter%inductive_term = 2.6e-9
      allocate(expected%mtln%connectors(4)%transfer_impedance_per_meter%poles(0))
      allocate(expected%mtln%connectors(4)%transfer_impedance_per_meter%residues(0))

      !cables
      allocate(expected%mtln%cables(9))
      ! level 0
      ! cable 1 - wire
      expected%mtln%cables(1)%name = "line_0_0"
      allocate(expected%mtln%cables(1)%inductance_per_meter(1,1))
      allocate(expected%mtln%cables(1)%capacitance_per_meter(1,1))
      allocate(expected%mtln%cables(1)%resistance_per_meter(1,1))
      allocate(expected%mtln%cables(1)%conductance_per_meter(1,1))

      expected%mtln%cables(1)%inductance_per_meter = reshape( source = [5.481553487168089e-07], shape = [ 1,1 ] )
      expected%mtln%cables(1)%capacitance_per_meter = reshape( source = [2.0270004E-11], shape = [ 1,1 ] )
      expected%mtln%cables(1)%resistance_per_meter =  reshape(source=[22.9e-3], shape=[1,1])
      expected%mtln%cables(1)%conductance_per_meter = reshape(source=[0.0], shape=[1,1])
      
      allocate(expected%mtln%cables(1)%step_size())
      expected%mtln%cables(1)%step_size = [(0.1, i = 1, 9)]

      expected%mtln%cables(1)%transfer_impedance%direction = 0
      expected%mtln%cables(1)%transfer_impedance%resistive_term = 0.0
      expected%mtln%cables(1)%transfer_impedance%inductive_term = 0.0
      allocate(expected%mtln%cables(1)%transfer_impedance%poles(0))
      allocate(expected%mtln%cables(1)%transfer_impedance%residues(0))

      expected%mtln%cables(1)%parent_cable => null()
      expected%mtln%cables(1)%conductor_in_parent = 0
      expected%mtln%cables(1)%initial_connector => expected%mtln%connectors(1)
      expected%mtln%cables(1)%end_connector => null()
      ! cable 4 - wire
      expected%mtln%cables(4)%name = "line_0_1"
      allocate(expected%mtln%cables(4)%inductance_per_meter(1,1))
      allocate(expected%mtln%cables(4)%capacitance_per_meter(1,1))
      allocate(expected%mtln%cables(4)%resistance_per_meter(1,1))
      allocate(expected%mtln%cables(4)%conductance_per_meter(1,1))

      expected%mtln%cables(4)%inductance_per_meter = reshape( source = [6.482560773828984e-07], shape = [ 1,1 ] )
      expected%mtln%cables(4)%capacitance_per_meter = reshape( source = [1.7140003E-11], shape = [ 1,1 ] )
      expected%mtln%cables(4)%resistance_per_meter =  reshape(source=[11.8e-3], shape=[1,1])
      expected%mtln%cables(4)%conductance_per_meter = reshape(source=[0.0], shape=[1,1])
      
      allocate(expected%mtln%cables(4)%step_size())
      expected%mtln%cables(4)%step_size = [(0.1, i = 1, 8)]

      expected%mtln%cables(4)%transfer_impedance%direction = 0
      expected%mtln%cables(4)%transfer_impedance%resistive_term = 0.0
      expected%mtln%cables(4)%transfer_impedance%inductive_term = 0.0
      allocate(expected%mtln%cables(4)%transfer_impedance%poles(0))
      allocate(expected%mtln%cables(4)%transfer_impedance%residues(0))

      expected%mtln%cables(4)%parent_cable => null()
      expected%mtln%cables(4)%conductor_in_parent = 0
      expected%mtln%cables(4)%initial_connector => => expected%mtln%connectors(2)
      expected%mtln%cables(4)%end_connector => null()
      ! cable 7 - wire
      expected%mtln%cables(7)%name = "line_0_2"
      allocate(expected%mtln%cables(7)%inductance_per_meter(1,1))
      allocate(expected%mtln%cables(7)%capacitance_per_meter(1,1))
      allocate(expected%mtln%cables(7)%resistance_per_meter(1,1))
      allocate(expected%mtln%cables(7)%conductance_per_meter(1,1))

      expected%mtln%cables(7)%inductance_per_meter = reshape( source = [5.802145885361537e-07], shape = [ 1,1 ] )
      expected%mtln%cables(7)%capacitance_per_meter = reshape( source = [1.9150003E-11], shape = [ 1,1 ] )
      expected%mtln%cables(7)%resistance_per_meter =  reshape(source=[17.3e-3], shape=[1,1])
      expected%mtln%cables(7)%conductance_per_meter = reshape(source=[0.0], shape=[1,1])
      
      allocate(expected%mtln%cables(1)%step_size(7))
      expected%mtln%cables(7)%step_size = [(0.1, i = 1, 7)]

      expected%mtln%cables(7)%transfer_impedance%direction = 0
      expected%mtln%cables(7)%transfer_impedance%resistive_term = 0.0
      expected%mtln%cables(7)%transfer_impedance%inductive_term = 0.0
      allocate(expected%mtln%cables(7)%transfer_impedance%poles(0))
      allocate(expected%mtln%cables(7)%transfer_impedance%residues(0))

      expected%mtln%cables(7)%parent_cable => null()
      expected%mtln%cables(7)%conductor_in_parent = 0
      expected%mtln%cables(7)%initial_connector => null()
      expected%mtln%cables(7)%end_connector => null()

      ! level 1
      ! cable 2 - multiwire
      expected%mtln%cables(2)%name = "line_1_0"
      allocate(expected%mtln%cables(2)%inductance_per_meter(1,1))
      allocate(expected%mtln%cables(2)%capacitance_per_meter(1,1))
      allocate(expected%mtln%cables(2)%resistance_per_meter(1,1))
      allocate(expected%mtln%cables(2)%conductance_per_meter(1,1))
      expected%mtln%cables(2)%inductance_per_meter = reshape(source=[8.802075200000001e-08], shape=[1,1])
      expected%mtln%cables(2)%capacitance_per_meter = reshape(source=[5.5840010E-10], shape=[1,1])
      expected%mtln%cables(2)%resistance_per_meter = reshape(source=[3.9e-3], shape=[1,1])
      expected%mtln%cables(2)%conductance_per_meter = reshape(source=[0.0], shape=[1,1])
      allocate(expected%mtln%cables(2)%step_size(9))
      expected%mtln%cables(2)%step_size =  [(0.1, i = 1, 9)]

      expected%mtln%cables(2)%transfer_impedance%direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
      expected%mtln%cables(2)%transfer_impedance%resistive_term = 0.0
      expected%mtln%cables(2)%transfer_impedance%inductive_term = 8.9e-9
      allocate(expected%mtln%cables(2)%transfer_impedance%poles(0))
      allocate(expected%mtln%cables(2)%transfer_impedance%residues(0))

      expected%mtln%cables(2)%parent_cable => expected%mtln%cables(1)
      expected%mtln%cables(2)%conductor_in_parent = 1
      expected%mtln%cables(2)%initial_connector => expected%mtln%connectors(3)
      expected%mtln%cables(2)%end_connector => null()

      ! cable 5 - multiwire
      expected%mtln%cables(5)%name = "line_1_1"
      allocate(expected%mtln%cables(5)%inductance_per_meter(1,1))
      allocate(expected%mtln%cables(5)%capacitance_per_meter(1,1))
      allocate(expected%mtln%cables(5)%resistance_per_meter(1,1))
      allocate(expected%mtln%cables(5)%conductance_per_meter(1,1))
      expected%mtln%cables(5)%inductance_per_meter = reshape(source=[1.37228e-07], shape=[1,1])
      expected%mtln%cables(5)%capacitance_per_meter = reshape(source=[3.2310005E-10], shape=[1,1])
      expected%mtln%cables(5)%resistance_per_meter = reshape(source=[12.2e-3,], shape=[1,1])
      expected%mtln%cables(5)%conductance_per_meter = reshape(source=[0.0], shape=[1,1])
      allocate(expected%mtln%cables(5)%step_size(8))
      expected%mtln%cables(5)%step_size =  [(0.1, i = 1, 8)]

      expected%mtln%cables(5)%transfer_impedance%direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
      expected%mtln%cables(5)%transfer_impedance%resistive_term = 0.0
      expected%mtln%cables(5)%transfer_impedance%inductive_term = 7.4e-9
      allocate(expected%mtln%cables(5)%transfer_impedance%poles(0))
      allocate(expected%mtln%cables(5)%transfer_impedance%residues(0))

      expected%mtln%cables(5)%parent_cable => expected%mtln%cables(4)
      expected%mtln%cables(5)%conductor_in_parent = 1
      expected%mtln%cables(5)%initial_connector => expected%mtln%connectors(4)
      expected%mtln%cables(5)%end_connector => null()

      ! cable 8 - multiwire
      expected%mtln%cables(8)%name = "line_1_2"
      allocate(expected%mtln%cables(8)%inductance_per_meter(1,1))
      allocate(expected%mtln%cables(8)%capacitance_per_meter(1,1))
      allocate(expected%mtln%cables(8)%resistance_per_meter(1,1))
      allocate(expected%mtln%cables(8)%conductance_per_meter(1,1))
      expected%mtln%cables(8)%inductance_per_meter = reshape(source=[9.1890502e-08], shape=[1,1])
      expected%mtln%cables(8)%capacitance_per_meter = reshape(source=[4.7190007E-10], shape=[1,1])
      expected%mtln%cables(8)%resistance_per_meter = reshape(source=[6.5e-3], shape=[1,1])
      expected%mtln%cables(8)%conductance_per_meter = reshape(source=[0.0], shape=[1,1])
      allocate(expected%mtln%cables(8)%step_size(7))
      expected%mtln%cables(8)%step_size =  [(0.1, i = 1, 7)]

      expected%mtln%cables(8)%transfer_impedance%direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
      expected%mtln%cables(8)%transfer_impedance%resistive_term = 0.0
      expected%mtln%cables(8)%transfer_impedance%inductive_term = 3.0e-9
      allocate(expected%mtln%cables(8)%transfer_impedance%poles(0))
      allocate(expected%mtln%cables(8)%transfer_impedance%residues(0))

      expected%mtln%cables(8)%parent_cable => expected%mtln%cables(7)
      expected%mtln%cables(8)%conductor_in_parent = 1
      expected%mtln%cables(8)%initial_connector => null()
      expected%mtln%cables(8)%end_connector => null()

      ! level 2 : 3, 6, 9      
      ! cable 3 - multiwire
      expected%mtln%cables(3)%name = "line_2_0"
      allocate(expected%mtln%cables(3)%inductance_per_meter(8,8), source = 0.0)
      allocate(expected%mtln%cables(3)%capacitance_per_meter(8,8), source = 0.0)
      allocate(expected%mtln%cables(3)%resistance_per_meter(8,8), source = 0.0)
      allocate(expected%mtln%cables(3)%conductance_per_meter(8,8), source = 0.0)
      expected%mtln%cables(3)%inductance_per_meter(1:2,1:2) = & 
         reshape(source=[2.4382084E-07, 4.7377505E-08, 4.7377508E-08, 2.4382081E-07], shape=[2,2])
      expected%mtln%cables(3)%inductance_per_meter(3:4,3:4) = & 
         reshape(source=[2.4382084E-07, 4.7377505E-08, 4.7377508E-08, 2.4382081E-07], shape=[2,2])
      expected%mtln%cables(3)%inductance_per_meter(5:6,5:6) = & 
         reshape(source=[2.4382084E-07, 4.7377505E-08, 4.7377508E-08, 2.4382081E-07], shape=[2,2])
      expected%mtln%cables(3)%inductance_per_meter(7:8,7:8) = & 
         reshape(source=[2.4382084E-07, 4.7377505E-08, 4.7377508E-08, 2.4382081E-07], shape=[2,2])

      expected%mtln%cables(3)%capacitance_per_meter(1:2,1:2) = &
         reshape(source=[105.5e-12, -20.5e-12, -20.5e-12, 105.5e-12], shape=[2,2])
      expected%mtln%cables(3)%capacitance_per_meter(3:4,3:4) = &
         reshape(source=[105.5e-12, -20.5e-12, -20.5e-12, 105.5e-12], shape=[2,2])
      expected%mtln%cables(3)%capacitance_per_meter(5:6,5:6) = &
         reshape(source=[105.5e-12, -20.5e-12, -20.5e-12, 105.5e-12], shape=[2,2])
      expected%mtln%cables(3)%capacitance_per_meter(7:8,7:8) = &
         reshape(source=[105.5e-12, -20.5e-12, -20.5e-12, 105.5e-12], shape=[2,2])

      expected%mtln%cables(3)%resistance_per_meter(i:i) [62.0e-3, i = 1,8]

      allocate(expected%mtln%cables(3)%step_size(9))
      expected%mtln%cables(3)%step_size =  [(0.1, i = 1, 9)]

      expected%mtln%cables(3)%transfer_impedance%direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
      expected%mtln%cables(3)%transfer_impedance%resistive_term = 0.0
      expected%mtln%cables(3)%transfer_impedance%inductive_term = 4.2e-9
      allocate(expected%mtln%cables(3)%transfer_impedance%poles(0))
      allocate(expected%mtln%cables(3)%transfer_impedance%residues(0))

      expected%mtln%cables(3)%parent_cable => expected%mtln%cables(2)
      expected%mtln%cables(3)%conductor_in_parent = 1
      expected%mtln%cables(3)%initial_connector => null()
      expected%mtln%cables(3)%end_connector => null()

      ! cable 6 - multiwire
      expected%mtln%cables(6)%name = "line_2_4"
      allocate(expected%mtln%cables(6)%inductance_per_meter(2,2), source = 0.0)
      allocate(expected%mtln%cables(6)%capacitance_per_meter(2,2), source = 0.0)
      allocate(expected%mtln%cables(6)%resistance_per_meter(2,2), source = 0.0)
      allocate(expected%mtln%cables(6)%conductance_per_meter(2,2), source = 0.0)
      expected%mtln%cables(6)%inductance_per_meter(1:2,1:2) = & 
         reshape(source=[2.4382084E-07, 4.7377505E-08, 4.7377508E-08, 2.4382081E-07], shape=[2,2])

      expected%mtln%cables(6)%capacitance_per_meter(1:2,1:2) = &
         reshape(source=[105.5e-12, -20.5e-12, -20.5e-12, 105.5e-12], shape=[2,2])

      expected%mtln%cables(6)%resistance_per_meter(i:i) [62.0e-3, i = 1,2]

      allocate(expected%mtln%cables(6)%step_size(9))
      expected%mtln%cables(6)%step_size =  [(0.1, i = 1, 9)]

      expected%mtln%cables(6)%transfer_impedance%direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
      expected%mtln%cables(6)%transfer_impedance%resistive_term = 0.0
      expected%mtln%cables(6)%transfer_impedance%inductive_term = 4.2e-9
      allocate(expected%mtln%cables(6)%transfer_impedance%poles(0))
      allocate(expected%mtln%cables(6)%transfer_impedance%residues(0))

      expected%mtln%cables(6)%parent_cable => expected%mtln%cables(5)
      expected%mtln%cables(6)%conductor_in_parent = 1
      expected%mtln%cables(6)%initial_connector => null()
      expected%mtln%cables(6)%end_connector => null()

      ! cable 9 - multiwire
      expected%mtln%cables(3)%name = "line_2_5"
      allocate(expected%mtln%cables(3)%inductance_per_meter(6,6), source = 0.0)
      allocate(expected%mtln%cables(3)%capacitance_per_meter(6,6), source = 0.0)
      allocate(expected%mtln%cables(3)%resistance_per_meter(6,6), source = 0.0)
      allocate(expected%mtln%cables(3)%conductance_per_meter(6,6), source = 0.0)
      expected%mtln%cables(3)%inductance_per_meter(1:2,1:2) = & 
         reshape(source=[2.4382084E-07, 4.7377505E-08, 4.7377508E-08, 2.4382081E-07], shape=[2,2])
      expected%mtln%cables(3)%inductance_per_meter(3:4,3:4) = & 
         reshape(source=[2.4382084E-07, 4.7377505E-08, 4.7377508E-08, 2.4382081E-07], shape=[2,2])
      expected%mtln%cables(3)%inductance_per_meter(5:6,5:6) = & 
         reshape(source=[2.4382084E-07, 4.7377505E-08, 4.7377508E-08, 2.4382081E-07], shape=[2,2])
      expected%mtln%cables(3)%inductance_per_meter(7:8,7:8) = & 
         reshape(source=[2.4382084E-07, 4.7377505E-08, 4.7377508E-08, 2.4382081E-07], shape=[2,2])

      expected%mtln%cables(3)%capacitance_per_meter(1:2,1:2) = &
         reshape(source=[105.5e-12, -20.5e-12, -20.5e-12, 105.5e-12], shape=[2,2])
      expected%mtln%cables(3)%capacitance_per_meter(3:4,3:4) = &
         reshape(source=[105.5e-12, -20.5e-12, -20.5e-12, 105.5e-12], shape=[2,2])
      expected%mtln%cables(3)%capacitance_per_meter(5:6,5:6) = &
         reshape(source=[105.5e-12, -20.5e-12, -20.5e-12, 105.5e-12], shape=[2,2])
      expected%mtln%cables(3)%capacitance_per_meter(7:8,7:8) = &
         reshape(source=[105.5e-12, -20.5e-12, -20.5e-12, 105.5e-12], shape=[2,2])

      expected%mtln%cables(3)%resistance_per_meter(i:i) [62.0e-3, i = 1,8]

      allocate(expected%mtln%cables(3)%step_size(9))
      expected%mtln%cables(3)%step_size =  [(0.1, i = 1, 9)]

      expected%mtln%cables(3)%transfer_impedance%direction = TRANSFER_IMPEDANCE_DIRECTION_INWARDS
      expected%mtln%cables(3)%transfer_impedance%resistive_term = 0.0
      expected%mtln%cables(3)%transfer_impedance%inductive_term = 4.2e-9
      allocate(expected%mtln%cables(3)%transfer_impedance%poles(0))
      allocate(expected%mtln%cables(3)%transfer_impedance%residues(0))

      expected%mtln%cables(3)%parent_cable => expected%mtln%cables(2)
      expected%mtln%cables(3)%conductor_in_parent = 1
      expected%mtln%cables(3)%initial_connector => null()
      expected%mtln%cables(3)%end_connector => null()


      ! probes
      allocate(expected%mtln%probes(2))
      expected%mtln%probes(1)%attached_to_cable => expected%mtln%cables(2) ! to which cable is the probe attached in mtln?
      expected%mtln%probes(1)%index = 19
      expected%mtln%probes(1)%probe_type = PROBE_TYPE_VOLTAGE

      expected%mtln%probes(2)%attached_to_cable => expected%mtln%cables(2)
      expected%mtln%probes(2)%index = 19
      expected%mtln%probes(2)%probe_type = PROBE_TYPE_CURRENT

      ! networks
      allocate(expected%mtln%networks(2))
      allocate(expected%mtln%networks(1)%connections(3))

      allocate(expected%mtln%networks(1)%connections(3)%nodes(1))
      expected%mtln%networks(1)%connections(3)%nodes(1)%conductor_in_cable = 1
      expected%mtln%networks(1)%connections(3)%nodes(1)%side = TERMINAL_NODE_SIDE_INI
      expected%mtln%networks(1)%connections(3)%nodes(1)%belongs_to_cable =>  expected%mtln%cables(2)
      allocate(termination_t :: expected%mtln%networks(1)%connections(3)%nodes(1)%termination)
      expected%mtln%networks(1)%connections(3)%nodes(1)%termination%termination_type = TERMINATION_SERIES
      expected%mtln%networks(1)%connections(3)%nodes(1)%termination%resistance = 50.0

      allocate(expected%mtln%networks(1)%connections(1)%nodes(1))
      expected%mtln%networks(1)%connections(1)%nodes(1)%conductor_in_cable = 1
      expected%mtln%networks(1)%connections(1)%nodes(1)%side = TERMINAL_NODE_SIDE_INI
      expected%mtln%networks(1)%connections(1)%nodes(1)%belongs_to_cable => expected%mtln%cables(1)
      allocate(termination_t :: expected%mtln%networks(1)%connections(1)%nodes(1)%termination)
      expected%mtln%networks(1)%connections(1)%nodes(1)%termination%termination_type = TERMINATION_SERIES
      expected%mtln%networks(1)%connections(1)%nodes(1)%termination%resistance = 50.0

      allocate(expected%mtln%networks(1)%connections(2)%nodes(1))
      expected%mtln%networks(1)%connections(2)%nodes(1)%conductor_in_cable = 2
      expected%mtln%networks(1)%connections(2)%nodes(1)%side = TERMINAL_NODE_SIDE_INI
      expected%mtln%networks(1)%connections(2)%nodes(1)%belongs_to_cable => expected%mtln%cables(1)
      allocate(termination_t :: expected%mtln%networks(1)%connections(2)%nodes(1)%termination)
      expected%mtln%networks(1)%connections(2)%nodes(1)%termination%termination_type = TERMINATION_SERIES
      expected%mtln%networks(1)%connections(2)%nodes(1)%termination%resistance = 50.0

      allocate(expected%mtln%networks(2)%connections(3))

      allocate(expected%mtln%networks(2)%connections(3)%nodes(1))
      expected%mtln%networks(2)%connections(3)%nodes(1)%conductor_in_cable = 1
      expected%mtln%networks(2)%connections(3)%nodes(1)%side = TERMINAL_NODE_SIDE_END
      expected%mtln%networks(2)%connections(3)%nodes(1)%belongs_to_cable => expected%mtln%cables(2)
      allocate(termination_t :: expected%mtln%networks(2)%connections(3)%nodes(1)%termination)
      expected%mtln%networks(2)%connections(3)%nodes(1)%termination%termination_type = TERMINATION_SERIES
      expected%mtln%networks(2)%connections(3)%nodes(1)%termination%resistance = 50.0

      allocate(expected%mtln%networks(2)%connections(1)%nodes(1))
      expected%mtln%networks(2)%connections(1)%nodes(1)%conductor_in_cable = 1
      expected%mtln%networks(2)%connections(1)%nodes(1)%side = TERMINAL_NODE_SIDE_END
      expected%mtln%networks(2)%connections(1)%nodes(1)%belongs_to_cable => expected%mtln%cables(1)
      allocate(termination_t :: expected%mtln%networks(2)%connections(1)%nodes(1)%termination)
      expected%mtln%networks(2)%connections(1)%nodes(1)%termination%termination_type = TERMINATION_SERIES
      expected%mtln%networks(2)%connections(1)%nodes(1)%termination%resistance = 50.0

      allocate(expected%mtln%networks(2)%connections(2)%nodes(1))
      expected%mtln%networks(2)%connections(2)%nodes(1)%conductor_in_cable = 2
      expected%mtln%networks(2)%connections(2)%nodes(1)%side = TERMINAL_NODE_SIDE_END
      expected%mtln%networks(2)%connections(2)%nodes(1)%belongs_to_cable => expected%mtln%cables(1)
      allocate(termination_t :: expected%mtln%networks(2)%connections(2)%nodes(1)%termination)
      expected%mtln%networks(2)%connections(2)%nodes(1)%termination%termination_type = TERMINATION_SERIES
      expected%mtln%networks(2)%connections(2)%nodes(1)%termination%resistance = 50.0




   end function
end function

