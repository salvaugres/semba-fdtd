import pytest
from src_pyWrapper.pyWrapper import *
import shutil, glob, re
import json

TEST_DATA_FOLDER = 'testData/'
CASE_FOLDER = TEST_DATA_FOLDER + 'case/'
EXCTITATIONS_FOLDER = TEST_DATA_FOLDER + 'gauss.exc'

SEMBA_EXE = 'build/bin/semba-fdtd'

def getCase(case):
    return json.load(open(CASE_FOLDER + case + '.fdtd.json'))

def makeTemporaryCopy(temp_dir, file_path):
    file_name = file_path.split('/')[-1]
    temp_path = os.path.join(temp_dir, file_name)
    shutil.copy2(file_path, temp_path)

def copyTemporaryInputFiles(temp_dir, input, excitation, executable):
    makeTemporaryCopy(temp_dir, input)
    makeTemporaryCopy(temp_dir, excitation)
    makeTemporaryCopy(temp_dir, executable)

def getProbeFile(prefix, probe_name):
    return  ([x for x in glob.glob('*dat') if re.match(prefix + '_' + probe_name + '.*dat',x)])[0]

def countLinesInOutputFile(prefix, probe_name):
    probe_file_name = getProbeFile(prefix, probe_name)
    probe_file = open(probe_file_name)
    num_lines= len(probe_file.readlines())
    probe_file.close()
    return num_lines
    
def readWithoutHeader(file_name):
    with open(file_name) as f:
        _ = f.readline()
        rest = f.read()
    return rest
    
def compareFiles(expected_name, result_name):
    f_expected = readWithoutHeader(expected_name)
    f_result = readWithoutHeader(result_name)
    return f_expected == f_result
    
    