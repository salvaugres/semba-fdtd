from utils import *

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
    wrapper = pyw.pyWrapper(file_name = input_name, path_to_exe= './')
    wrapper.run()

    j_dict = wrapper.createJsonDict()
    if "probes" in j_dict:
        for probe in j_dict["probes"]:
            assert(isProbeInOutputFiles(input_name.split('.json')[0], probe["name"]) == True)
            expected_probe = getProbeFile('base_case_'+ input_name.split('.json')[0], probe["name"])
            result_probe = getProbeFile(input_name.split('.json')[0], probe["name"])
            assert(compareFiles(expected_probe, result_probe))

    assert(wrapper.hasFinishedSuccess() == True)
    
@pytest.mark.skip(reason="Todo")
def test_towel_hanger_mpi(tmp_path):
    # ToDo
    assert False