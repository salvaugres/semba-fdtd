from utils import *

def test_probes_output_exists(tmp_path):
    case = 'holland1981'
    input_json = getCase(case)
    input_json['general']['numberOfTimeSteps'] = 1
    input_file = tmp_path + case + '.fdtd.json'
    input_json.save(input_file)    

    makeTemporaryCopy(tmp_path, EXCTITATIONS_FOLDER+'gauss.exc')

    solver = FDTD(file_name = input_file, path_to_exe=SEMBA_EXE)
    
    solver.run()
    
    probe_files = solver.getSolvedProbeFiles("mid_point")
    
    assert solver.hasFinishedSuccessfully() == True
    assert len(probe_files) == 1
    assert 'holland1981.fdtd_mid_point_Wz_11_11_12_s2.dat' == probe_files[0]
           
    

def test_probes_output_number_of_steps(tmp_path):
    input_name = 'holland1981_1_step.fdtd.json'

    input_path = CASE_FOLDER + input_name
    excitation_path = CASE_FOLDER + 'gauss.exc'
    executable_path = 'build/bin/semba-fdtd'

    copyTemporaryInputFiles(tmp_path, input_path, excitation_path, executable_path)
    os.chdir(tmp_path)
    wrapper = pyw.FDTD(file_name = input_name, path_to_exe= './')
    wrapper.run()

    j_dict = wrapper.createJsonDict()
    if "probes" in j_dict:
        for probe in j_dict["probes"]:
            assert(isProbeInOutputFiles(input_name.split('.json')[0], probe["name"]) == True)
            assert(countLinesInOutputFile(input_name.split('.json')[0], probe["name"]) == 3)

    assert(wrapper.hasFinishedSuccess() == True)


def test_towel_hanger(tmp_path):

    CASE_FOLDER = CASES_FOLDER + 'towel_hanger/base_case/'
    input_name = 'towel_hanger.fdtd.json'

    input_path = CASE_FOLDER + input_name
    excitation_path = CASE_FOLDER + 'gauss.exc'
    executable_path = 'build/bin/semba-fdtd'
   
    expected_path = CASE_FOLDER+'base_case_towel_hanger.fdtd_*****.dat'

    copyTemporaryInputFiles(tmp_path, input_path, excitation_path, executable_path)
    makeTemporaryCopy(tmp_path, expected_path)
    os.chdir(tmp_path)

    #run the case
    wrapper = pyw.FDTD(file_name = input_name, path_to_exe= './')
    wrapper.run()

    j_dict = wrapper.createJsonDict()
    if "probes" in j_dict:
        for probe in j_dict["probes"]:
            assert(isProbeInOutputFiles(input_name.split('.json')[0], probe["name"]) == True)
            expected_probe = getProbeFile('base_case_'+ input_name.split('.json')[0], probe["name"])
            result_probe = getProbeFile(input_name.split('.json')[0], probe["name"])
            assert(compareFiles(expected_probe, result_probe))

    assert(wrapper.hasFinishedSuccess() == True)
    

def test_holland(tmp_path):

    CASE_FOLDER = CASES_FOLDER + 'holland/base_case/'
    input_name = 'holland1981.fdtd.json'

    input_path = CASE_FOLDER + input_name
    excitation_path = CASE_FOLDER + 'gauss.exc'
    executable_path = 'build/bin/semba-fdtd'
   
    expected_path = CASE_FOLDER+'base_case_holland1981.fdtd_mid_point_Wz_11_11_12_s2.dat'

    copyTemporaryInputFiles(tmp_path, input_path, excitation_path, executable_path)
    makeTemporaryCopy(tmp_path, expected_path)
    
    os.chdir(tmp_path)
    wrapper = pyw.FDTD(file_name = input_name, path_to_exe= './')
    wrapper.run()

    j_dict = wrapper.createJsonDict()
    if "probes" in j_dict:
        for probe in j_dict["probes"]:
            assert(isProbeInOutputFiles(input_name.split('.json')[0], probe["name"]) == True)
            expected_probe = getProbeFile('base_case_'+ input_name.split('.json')[0], probe["name"])
            result_probe = getProbeFile(input_name.split('.json')[0], probe["name"])
            assert(compareFiles(expected_probe, result_probe))

    assert(wrapper.hasFinishedSuccess() == True)
    