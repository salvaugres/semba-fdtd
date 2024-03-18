import subprocess
import json


class pyWrapper():
    def __init__(self, file_name, path_to_exe):
        self.file_name = file_name
        self.path_to_exe = path_to_exe
        self.input_file = open(self.file_name)
    
    def run(self):
        self.output = subprocess.run([self.path_to_exe, "-i",self.file_name])
    
    
    def createJsonDict(self):
        return json.load(self.input_file)
    
    def hasFinishedSuccess(self):
        if (self.output.returncode == 0):
            return True
        else:
            return False