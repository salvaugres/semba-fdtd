from utils import *

def test_holland(tmp_path):
    case = 'holland1981'
    makeTemporaryCopy(tmp_path, EXCITATIONS_FOLDER+'gauss.exc')
    makeTemporaryCopy(tmp_path, CASE_FOLDER + case + '.fdtd.json')
    fn = tmp_path._str + '/' + case + '.fdtd.json'

    solver = FDTD(input_filename = fn, path_to_exe=SEMBA_EXE)
    solver.run()
    probe_files = solver.getSolvedProbeFilenames("mid_point")
    
    assert solver.hasFinishedSuccessfully() == True
    assert len(probe_files) == 1
    assert 'holland1981.fdtd_mid_point_Wz_11_11_12_s2.dat' == probe_files[0]
    assert countLinesInFile(probe_files[0]) == 1002
    # assert compareFiles(OUTPUT_FOLDER+'holland1981.fdtd_mid_point_Wz_11_11_12_s2.dat',\
    #                     probe_files[0])

    
def test_sphere(tmp_path):    
    case = 'sphere'
    input_json = getCase(case)
    input_json['general']['numberOfSteps'] = 1
    input_json['general']['timeStep'] = 3.0E-013
    
    fn = tmp_path._str + '/' + case + '.fdtd.json'
    with open(fn, 'w') as modified_json:
        json.dump(input_json, modified_json) 

    makeTemporaryCopy(tmp_path, EXCITATIONS_FOLDER+'gauss.exc')

    solver = FDTD(input_filename = fn, path_to_exe=SEMBA_EXE)
    solver.run()
    probe_files = solver.getSolvedProbeFilenames("Far") # semba-fdtd seems to always use the name Far for "far field" probes.
    
    assert solver.hasFinishedSuccessfully() == True
    assert len(probe_files) == 1
    assert 'sphere.fdtd_Far_FF_2_2_2__77_77_77.dat' == probe_files[0]