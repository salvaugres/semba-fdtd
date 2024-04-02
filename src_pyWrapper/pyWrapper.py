import subprocess
import json
import os
import glob, re

class FDTD():
    def __init__(self, file_name, path_to_exe):
        self.file_name = file_name
        self.path_to_exe = path_to_exe

        self.folder = os.path.dirname(self.file_name)
        self.case = os.path.basename(self.file_name).split('.json')[0]
        self.hasRun = False
    
    def run(self):
        self.wd = os.getcwd()+'/'
        os.chdir(self.folder)
        self.output = subprocess.run([self.wd+self.path_to_exe, "-i",self.file_name])
        self.hasRun = True
    
    def readJsonDict(self):
        with open(self.file_name) as input_file:
            return json.load(input_file)
        
    def getSolvedProbeFilenames(self, probe_name):
        input_json = self.readJsonDict()
        if not "probes" in input_json:
            raise ValueError('Solver does not contain probes.')
        
        for probe in input_json["probes"]:
            probeFiles = [x for x in glob.glob('*dat') if re.match(self.case + '_' + probe_name + '.*dat',x)]
            return probeFiles
    
    def hasFinishedSuccessfully(self):
        if self.hasRun:
            if (self.output.returncode == 0):
                return True
            else:
                return False
        
        
