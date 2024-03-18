import json
# from pytest import *
# import numpy as np

import src_pyWrapper.pyWrapper as pyw
import shutil, os, glob, tempfile, re

TEST_DATA_FOLDER = 'testData/'
CASES_FOLDER = TEST_DATA_FOLDER+'cases/'

def makeTemporaryCopy(temp_dir, file_path):
    file_name = file_path.split('/')[-1]
    temp_path = os.path.join(temp_dir, file_name)
    shutil.copy2(file_path, temp_path)
    return temp_path

def isProbeInOutputFiles(prefix, probe_name):
    txt_files = glob.glob('*dat')
    return len([x for x in txt_files if re.search(prefix + '_' + probe_name['name'] + '.*dat',x)]) == 1
    

def test_init():

    input_name = 'holland1981.fdtd.json'
    input_path = CASES_FOLDER+input_name

    with tempfile.TemporaryDirectory() as temp_dir:
            
        input_copy = makeTemporaryCopy(temp_dir, input_path)
        wrapper = pyw.pyWrapper(file_name = input_copy,\
                                path_to_exe= "build/bin/semba-fdtd")
        wrapper.run()

        j_dict = wrapper.createJsonDict()
        if "probes" in j_dict:
            for probe in j_dict["probes"]:
                assert(isProbeInOutputFiles(input_name.split('.json')[0], probe) == True)

        assert(wrapper.hasFinishedSuccess == True)
    
    