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
        dsetFilename = "pp2_pohnpei_" + currentFile.name[17:25] + ".nc"

        print(dsetFilename)

        toWriteDataset = initNetCDF(dsetFilename)
        wholeFile = currentFile.readlines()
        
        temperatures = []
        conductivities = []
        chlorophylls = []
        turbidities = []
        salinities = []
        dates = []

        for l in wholeFile:
            splitLine = l.split(',')
            temperatures.append(splitLine[0][2:9])
            conductivities.append(splitLine[1][2:])
            chlorophylls.append

        toWriteDataset.close()


# Initialize the netCDF file with the correct dimensions and all that. No data to be added just yet
# Input: date
# Output: netCDF file pointer
def initNetCDF(dsetFilename):

    dataSet = Dataset(dsetFilename, "w", format="NETCDF4")
    dataSet.createDimension("time", None)
    dataSet.createDimension("z", 1)
    dataSet.createDimension("lat", 1)
    dataSet.createDimension("lon", 1)

    return dataSet






 

if __name__ == "__main__":
    main()




