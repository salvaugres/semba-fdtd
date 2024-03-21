from utils import *

def test_probes_output_exists(tmp_path):


    CASE_FOLDER = CASES_FOLDER+'check_probes/'

    input_name = 'holland1981_1_step.fdtd.json'

    input_path = CASE_FOLDER + input_name
    excitation_path = CASE_FOLDER + 'gauss.exc'
    executable_path = 'build/bin/semba-fdtd'

    copyTemporaryInputFiles(tmp_path, input_path, excitation_path, executable_path)
    os.chdir(tmp_path)
    wrapper = pyw.pyWrapper(file_name = input_name, path_to_exe= './')
    wrapper.run()

    j_dict = wrapper.createJsonDict()
    if "probes" in j_dict:
        for probe in j_dict["probes"]:
            assert(isProbeInOutputFiles(input_name.split('.json')[0], probe["name"]) == True)

    assert(wrapper.hasFinishedSuccess() == True)

def test_probes_output_number_of_steps(tmp_path):

    CASE_FOLDER = CASES_FOLDER+'check_probes/'

    input_name = 'holland1981_1_step.fdtd.json'

    input_path = CASE_FOLDER + input_name
    excitation_path = CASE_FOLDER + 'gauss.exc'
    executable_path = 'build/bin/semba-fdtd'

    copyTemporaryInputFiles(tmp_path, input_path, excitation_path, executable_path)
    os.chdir(tmp_path)
    wrapper = pyw.pyWrapper(file_name = input_name, path_to_exe= './')
    wrapper.run()

    j_dict = wrapper.createJsonDict()
    if "probes" in j_dict:
        for probe in j_dict["probes"]:
            assert(isProbeInOutputFiles(input_name.split('.json')[0], probe["name"]) == True)
            assert(countLinesInOutputFile(input_name.split('.json')[0], probe["name"]) == 3)

    assert(wrapper.hasFinishedSuccess() == True)

