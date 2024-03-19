import pytest
import src_pyWrapper.pyWrapper as pyw
import shutil, os, glob, re

TEST_DATA_FOLDER = 'testData/'
CASES_FOLDER = TEST_DATA_FOLDER+'pyWrapper/'

def makeTemporaryCopy(temp_dir, file_path):
    file_name = file_path.split('/')[-1]
    temp_path = os.path.join(temp_dir, file_name)
    shutil.copy2(file_path, temp_path)

def copyTemporaryInputFiles(temp_dir, input, excitation, executable):
    makeTemporaryCopy(temp_dir, input)
    makeTemporaryCopy(temp_dir, excitation)
    makeTemporaryCopy(temp_dir, executable)

def isProbeInOutputFiles(prefix, probe_name):
    return len([x for x in glob.glob('*dat') if re.match(prefix + '_' + probe_name + '.*dat',x)]) == 1
    # r = [x for x in glob.glob('*dat') if re.match(prefix + '_' + probe_name + '.*dat',x)]
    # return len(r) == 1

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
    
    