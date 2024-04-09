import subprocess
import json
import os
import glob, re
import pandas as pd
import numpy as np

class Probe():
    
    def __init__(self, probe_filename):
        self.filename = probe_filename
        
        current_probe_tags = ('_Wx_', '_Wy_', '_Wz_')
        
        basename = os.path.basename(self.filename)
        self.case_name, basename_with_no_case_name = basename.split('.fdtd_')
        basename_with_no_case_name = basename_with_no_case_name.split('.dat')[0]
        
        for tag in current_probe_tags:
            ids = [m.start() for m in re.finditer(tag, basename_with_no_case_name)]
            if len(ids) == 0:
                continue
            elif len(ids) == 1:
                self.type = 'wire'
                self.name, position_str = basename_with_no_case_name.split(tag)
                break
            else:
                raise ValueError("Unable to determine probe name or type for a probe with name:" + basename)
        try:
            self.type
            self.name
        except:
            raise ValueError('Unable to determine type for probe' + basename)
        
        pos = position_str.split('_')
        self.cell = np.array([int(pos[0]), int(pos[1]), int(pos[2])])
        self.segment_tag = int(pos[3].split('s')[1])

        self.df = pd.read_csv(self.filename, sep='\s+')
        
        if self.type == 'wire':
            self.df = self.df.rename(columns={'t': 'time', basename: 'current'})
    
    def __getitem__(self, key):
        return self.df[key]

class FDTD():
    def __init__(self, input_filename, path_to_exe):
        self.filename = input_filename
        self.path_to_exe = path_to_exe

        self.folder = os.path.dirname(self.filename)
        self.case = os.path.basename(self.filename).split('.json')[0]
        self.hasRun = False
    
    def run(self):
        os.chdir(self.folder)
        self.output = subprocess.run([self.path_to_exe, "-i",self.filename])
        self.hasRun = True
    
    def readJsonDict(self):
        with open(self.filename) as input_file:
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
        
        
