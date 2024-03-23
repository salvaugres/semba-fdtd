integer function test_json_to_solver_input_shielded_pair() bind (C) result(err)
   use smbjson
   use testingTools
   use mtln_solver_mod, mtln_solver_t => mtln_t
   
   implicit none

   character(len=*),parameter :: filename = PATH_TO_TEST_DATA//'system/mtln.fdtd.json'
   type(Parseador) :: problem
   type(parser_t) :: parser
   type(mtln_solver_t) :: solver
   type(preprocess_t) :: pre, expected
   logical :: areSame
   err = 0

   parser = parser_t(filename)
   problem = parser%readProblemDescription()
   
   expected = expectedPreprocess()
   pre = preprocess(problem%mtln)

   ! solver = mtlnCtor(problem%mtln)
   ! call expect_eq(err, expected, problem)
   ! call expect_eq_mtln(err, expected, problem)

contains
   function expectedPreprocess() result (expected)
      type(preprocess_t) :: expected
      integer :: i
      expected%final_time = 1e-9
      expected%dt = 1e-12

      !BUNDLES
      allocate(expected%bundles(1))
      expected%bundles(1)%name =  "bundle_line_0"
      allocate(expected%bundles(1)%lpul(3,3,18), source = 0.0)
      allocate(expected%bundles(1)%cpul(3,3,19), source = 0.0)
      allocate(expected%bundles(1)%rpul(3,3,18), source = 0.0)
      allocate(expected%bundles(1)%gpul(3,3,19), source = 0.0)
      
      expected%bundles(1)%lpul(1,1,:) = 5.362505362505362e-07
      do i = 1, 18
         expected%bundles(1)%lpul(2:3,2:3,i) = & 
            reshape(source=[3.13182309e-07, 7.45674981e-08, 7.45674981e-08, 3.13182309e-07], shape=[2,2], order =[2,1])
      end do

      expected%bundles(1)%cpul(1,1,:) = 20.72e-12
      do i = 1, 19
         expected%bundles(1)%cpul(2:3,2:3,i) = & 
            reshape(source=[85.0e-12, -20.5e-12, -20.5e-12, 85.0e-12], shape=[2,2], order =[2,1])
      end do

      expected%bundles(1)%rpul(1,1,:) = 22.9e-3

      expected%bundles(1)%number_of_conductors = 3
      expected%bundles(1)%number_of_divisions = 18
      expected%bundles(1)%step_size =  [(0.03, i = 1, 18)]
      allocate(expected%bundles(1)%v(3, 19))
      allocate(expected%bundles(1)%i(3, 18))
      allocate(expected%bundles(1)%du(3, 3, 18))
      expected%bundles(1)%dt = 1e-12
      allocate(expected%bundles(1)%probes(0))
      
      expected%bundles(1)%transfer_impedance%dt = 1e-12
      expected%bundles(1)%transfer_impedance%number_of_conductors = 3
      expected%bundles(1)%transfer_impedance%number_of_divisions = 18
      expected%bundles(1)%transfer_impedance%number_of_poles = 0
      block
         complex :: zero 
         integer :: ndiv, ncond, npoles
         ndiv = 18
         ncond = 3
         npoles = 0
         zero%re = 0.0
         zero%im = 0.0
         allocate(expected%bundles(1)%transfer_impedance%phi(ndiv, ncond,npoles), source = zero)
         allocate(expected%bundles(1)%transfer_impedance%q1 (ndiv, ncond,ncond, npoles), source = zero)
         allocate(expected%bundles(1)%transfer_impedance%q2 (ndiv, ncond,ncond, npoles), source = zero)
         allocate(expected%bundles(1)%transfer_impedance%q3 (ndiv, ncond,ncond, npoles), source = zero)
         allocate(expected%bundles(1)%transfer_impedance%d  (ndiv, ncond,ncond), source = 0.0)
         allocate(expected%bundles(1)%transfer_impedance%e  (ndiv, ncond,ncond), source = 0.0)
         allocate(expected%bundles(1)%transfer_impedance%q1_sum (ndiv, ncond,ncond), source = zero)
         allocate(expected%bundles(1)%transfer_impedance%q2_sum (ndiv, ncond,ncond), source = zero)
         allocate(expected%bundles(1)%transfer_impedance%q3_phi (ndiv, ncond), source = zero)
      end block    

      expected%bundles(1)%conductors_in_level = (/1,2/)

      !NETWORKS
      allocate(expected%network_manager%networks(2))
      expected%network_manager%dt = 1e-12
      expected%network_manager%networks(1)%number_of_nodes = 3
      allocate(expected%network_manager%networks(1)%nodes(3))
      expected%network_manager%networks(1)%nodes(1)%bundle_number = 1
      expected%network_manager%networks(1)%nodes(1)%conductor_number = 1
      expected%network_manager%networks(1)%nodes(1)%v_index = 1
      expected%network_manager%networks(1)%nodes(1)%i_index = 1
      expected%network_manager%networks(1)%nodes(1)%line_c_per_meter = 20.72e-12
      expected%network_manager%networks(1)%nodes(2)%bundle_number = 1
      expected%network_manager%networks(1)%nodes(2)%conductor_number = 2
      expected%network_manager%networks(1)%nodes(2)%v_index = 1
      expected%network_manager%networks(1)%nodes(2)%i_index = 1
      expected%network_manager%networks(1)%nodes(2)%line_c_per_meter = 85.0e-12
      expected%network_manager%networks(1)%nodes(3)%bundle_number = 1
      expected%network_manager%networks(1)%nodes(3)%conductor_number = 3
      expected%network_manager%networks(1)%nodes(3)%v_index = 1
      expected%network_manager%networks(1)%nodes(3)%i_index = 1
      expected%network_manager%networks(1)%nodes(3)%line_c_per_meter = 85.0e-12
      allocate(expected%network_manager%networks(2)%nodes(3))
      expected%network_manager%networks(2)%nodes(1)%bundle_number = 1
      expected%network_manager%networks(2)%nodes(1)%conductor_number = 1
      expected%network_manager%networks(2)%nodes(1)%v_index = 18
      expected%network_manager%networks(2)%nodes(1)%i_index = 19
      expected%network_manager%networks(2)%nodes(1)%line_c_per_meter = 20.72e-12
      expected%network_manager%networks(2)%nodes(2)%bundle_number = 1
      expected%network_manager%networks(2)%nodes(2)%conductor_number = 2
      expected%network_manager%networks(2)%nodes(2)%v_index = 18
      expected%network_manager%networks(2)%nodes(2)%i_index = 19
      expected%network_manager%networks(2)%nodes(2)%line_c_per_meter = 85.0e-12
      expected%network_manager%networks(2)%nodes(3)%bundle_number = 1
      expected%network_manager%networks(2)%nodes(3)%conductor_number = 3
      expected%network_manager%networks(2)%nodes(3)%v_index = 18
      expected%network_manager%networks(2)%nodes(3)%i_index = 19
      expected%network_manager%networks(2)%nodes(3)%line_c_per_meter = 85.0e-12

      !PROBES
      allocate(expected%probes(2))
      expected%probes(1) = expected%bundles(1)%addProbe(index = 19, probe_type = PROBE_TYPE_VOLTAGE)
      expected%probes(2) = expected%bundles(1)%addProbe(index = 19, probe_type = PROBE_TYPE_CURRENT)
      !MAPS
      call expected%conductors_before_cable%set(key('line_1'), value = 1)
      call expected%cable_name_to_bundle_id%set(key('line_0'), value = 1)
      call expected%cable_name_to_bundle_id%set(key('line_1'), value = 1)
   end function


end function

