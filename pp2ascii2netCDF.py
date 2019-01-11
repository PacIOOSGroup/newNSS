from netCDF4 import Dataset
import scipy.io
import time
import datetime
import numpy as np
from numpy import array
import sys
import glob




def main():

    fileList = sorted(glob.glob("pp2data/byDayUTC/*"))

    for f in fileList:
        currentFile = open(f, 'r')

      




# Initialize the netCDF file with the correct dimensions and all that. No data to be added just yet
# Input: date
# Output: netCDF file pointer
def initNetCDF():


        dataSet = Dataset("test.nc", format="NETCDF4")
        dataSet.createDimension("time", 24)
        dataSet.createDImension("")






 

if __name__ == "__main__":
    main()




