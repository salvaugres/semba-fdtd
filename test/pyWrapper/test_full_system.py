from utils import *

def test_shielded_pair(tmp_path):
    case = "shieldedPair"

    makeCopy(tmp_path, CASE_FOLDER + case + '.fdtd.json')
    fn = tmp_path._str + '/' + case + '.fdtd.json'

    solver = FDTD(input_filename = fn, path_to_exe=SEMBA_EXE)
    solver.run()
    
    assert solver.hasFinishedSuccessfully() == True

    p_v_expected = Probe(OUTPUT_FOLDER+'shieldedPair.fdtd_mid_point_Wz_11_11_12_s2.dat')
    p_v_solved = Probe(solver.getSolvedProbeFilenames("voltage_at_wire_end")[0])

    # p_i_expected = Probe(OUTPUT_FOLDER+'shieldedPair.fdtd_mid_point_Wz_11_11_12_s2.dat')
    p_i_solved = Probe(solver.getSolvedProbeFilenames("current_at_wire_end")[0])
    
    assert np.allclose(p_v_expected.df.to_numpy(), p_v_solved.df.to_numpy())
    # assert np.allclose(p_i_expected.df.to_numpy(), p_i_solved.df.to_numpy())

def test_holland(tmp_path):
    case = 'holland1981'
    makeCopy(tmp_path, EXCITATIONS_FOLDER+'holland.exc')
    makeCopy(tmp_path, CASE_FOLDER + case + '.fdtd.json')
    fn = tmp_path._str + '/' + case + '.fdtd.json'

    solver = FDTD(input_filename = fn, path_to_exe=SEMBA_EXE)
    solver.run()
    probe_current = solver.getSolvedProbeFilenames("mid_point")[0]
    probe_files = [probe_current]
    
    assert solver.hasFinishedSuccessfully() == True

    p_expected = Probe(OUTPUT_FOLDER+'holland1981.fdtd_mid_point_Wz_11_11_12_s2.dat')
    
    p_solved = Probe(probe_files[0])
    assert np.allclose(p_expected.df.to_numpy()[:,0:3], p_solved.df.to_numpy()[:,0:3], rtol = 1e-5, atol=1e-6)

def test_towelHanger(tmp_path):
    case = 'towelHanger'
    makeCopy(tmp_path, EXCITATIONS_FOLDER+'towelHanger.exc')
    makeCopy(tmp_path, CASE_FOLDER + case + '.fdtd.json')
    fn = tmp_path._str + '/' + case + '.fdtd.json'

    solver = FDTD(input_filename = fn, path_to_exe=SEMBA_EXE)
    solver.run()
    probe_files = [solver.getSolvedProbeFilenames("wire_start")[0], 
                   solver.getSolvedProbeFilenames("wire_mid")[0], 
                   solver.getSolvedProbeFilenames("wire_end")[0]]
    
    assert solver.hasFinishedSuccessfully() == True

    p_expected = [Probe(OUTPUT_FOLDER+'towelHanger.fdtd_wire_start_Wz_27_25_30_s1.dat'),
                  Probe(OUTPUT_FOLDER+'towelHanger.fdtd_wire_mid_Wx_35_25_32_s5.dat'),
                  Probe(OUTPUT_FOLDER+'towelHanger.fdtd_wire_end_Wz_43_25_30_s4.dat')]
    
    for i in range(3):
        p_solved = Probe(probe_files[i])
        assert np.allclose(p_expected[i].df.to_numpy()[:,0:3], p_solved.df.to_numpy()[:,0:3], rtol = 5e-2, atol=5e-2)


    
def test_sphere(tmp_path):    
    case = 'sphere'
    input_json = getCase(case)
    input_json['general']['numberOfSteps'] = 200
    input_json['probes'][0]['domain']['numberOfFrequencies'] = 100
    
    fn = tmp_path._str + '/' + case + '.fdtd.json'
    with open(fn, 'w') as modified_json:
        json.dump(input_json, modified_json) 

    makeCopy(tmp_path, EXCITATIONS_FOLDER+'gauss.exc')

    solver = FDTD(input_filename = fn, path_to_exe=SEMBA_EXE)
    solver.run()
    probe_files = solver.getSolvedProbeFilenames("Far") # semba-fdtd seems to always use the name Far for "far field" probes.
    
    assert solver.hasFinishedSuccessfully() == True
    assert len(probe_files) == 1
    
    p = Probe(probe_files[0])
    assert p.type == 'farField'