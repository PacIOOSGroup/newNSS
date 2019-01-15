from netCDF4 import Dataset
import scipy.io
import time
import datetime
import numpy as np
from numpy import array
import sys
import glob
import datetime
import pytz





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
            temperatures.append(splitLine[0].strip('#').strip(' '))
            conductivities.append(splitLine[1].strip(' '))
            chlorophylls.append(splitLine[2].strip(' '))
            turbidities.append(splitLine[3].strip(' '))
            salinities.append(splitLine[4].strip(' '))
            
            date = datetime.datetime(int(splitLine[5][8:12]), monthToNumber(splitLine[5][4:7]), int(splitLine[5][1:3]), int(splitLine[5][13:15]), int(splitLine[5][16:18]), int(splitLine[5][19:21]), 0, pytz.timezone('Pacific/Pohnpei'))
            tz = pytz.utc
            date = date.astimezone(tz)           
            dates.append(date)
        


            

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

    timeVar = dataSet.createVariable("time", "f4", ("time"))
    timeVar.long_name="Time"
    timeVar.standard_name="time"
    timeVar.short_name="time"
    timeVar.axis="T"
    timeVar.units="minutes since 2008-01-01 00:00:00"

    zVar = dataSet.createVariable("z", "f4")
    zVar.long_name="depth below mean sea level"
    zVar.standard_name="depth"
    zVar.short_name="depth"
    zVar.axis="z"
    zVar.units="meters"

    latVar = dataSet.createVariable("lat","f4")
    latVar.long_name="Latitude"
    latVar.standard_name="latitude"
    latVar.short_name="lat"
    latVar.axis="Y"
    latVar.units="degrees_north"

    lonVar = dataSet.createVariable("lon","f4")
    lonVar.long_name="Longitude"
    lonVar.standard_name="longitude"
    lonVar.short_name="lon"
    lonVar.axis="X"
    lonVar.units="degrees_east"

    tempVar = dataSet.createVariable("temperature", "f4", ("time","z","lat","lon"))
    tempVar.long_name="Temperature"
    tempVar.standard_name="sea_water_temperature"
    tempVar.short_name="temp"
    tempVar.units="Celcius"

    conductivityVar = dataSet.createVariable("conductivity", "f4", ("time","z","lat","lon"))
    conductivityVar.long_name="Conductivity"
    conductivityVar.standard_name="sea_water_electrical_conductivity"
    conductivityVar.short_name="cond"
    conductivityVar.units="S m-1"

    chlorophyllVar = dataSet.createVariable("chlorophyll", "f4", ("time","z","lat","lon"))
    chlorophyllVar.long_name="Chlorophyll"
    chlorophyllVar.standard_name="chlorophyll_concentration_in_sea_water"
    chlorophyllVar.short_name="flor"
    chlorophyllVar.units = "kg m-3"
    
    turbidityVar = dataSet.createVariable("turbidity", "f4", ("time","z","lat","lon"))
    turbidityVar.long_name="Turbidity"
    turbidityVar.standard_name="turbidity_of_sea_water"
    turbidityVar.short_name="turb"
    turbidityVar.units="ntus"

    salinityVar = dataSet.createVariable("salinity", "f4", ("time","z","lat","lon"))
    salinityVar.long_name="Salinity"
    salinityVar.standard_name="sea_water_salinity"
    salinityVar.short_name="salt"
    salinityVar.units="1e-3"






    return dataSet

def monthToNumber(month):

    if month == "Jan":
        return 1
    elif month == "Feb":
        return 2
    elif month == "Mar":
        return 3
    elif month == "Apr":
        return 4
    elif month == "May":
        return 5
    elif month == "Jun":
        return 6
    elif month == "Jul":
        return 7
    elif month == "Aug":
        return 8
    elif month == "Sep":
        return 9
    elif month == "Oct":
        return 10
    elif month == "Nov":
        return 11
    elif month == "Dec":
        return 12
    else:
        return -1





if __name__ == "__main__":
    main()




