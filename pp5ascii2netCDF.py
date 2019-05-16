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

    fileList = sorted(glob.glob("pp5data/byDayUTC/*"))   
    #filelist = sorted(glob.glob("pp5data/byDayUTC/*"))
    for f in fileList:
        currentFile = open(f, 'r')
        dsetFilename = "pp5Outfiles/pp05_" + currentFile.name[17:21] + "_" + currentFile.name[21:23] + "_" + currentFile.name[23:25] + ".nc"
       # dsetFilename = "pp5Outfiles/pp05_big_file.nc"
        print(dsetFilename)

        toWriteDataset = initNetCDF(dsetFilename)
        wholeFile = currentFile.readlines()

        firstOf2008 = datetime.datetime.strptime('2008-01-01 00:00:00', '%Y-%m-%d %H:%M:%S')

        temperatures = []
        conductivities = []
        chlorophylls = []
        pressures = []
        turbidities = []
        salinities = []
        dates = []

        i = 0

        for l in wholeFile:
            splitLine = l.split(',')
            temperatures.append(float(splitLine[0].strip('#').strip(' ')))
            conductivities.append(float(splitLine[1].strip(' ')))
            pressures.append(float(splitLine[2].strip(' ')))
            chlorophylls.append(float(splitLine[3].strip(' ')))
            turbidities.append(float(splitLine[4].strip(' ')))
            salinities.append(float(splitLine[5].strip(' ')))
            splitLine[6] = splitLine[6].strip(' ')

            #i += 1
            #print(i/440)
            
            date = datetime.datetime(int(splitLine[6][7:11]), monthToNumber(splitLine[6][3:6]), int(splitLine[6][0:2]), int(splitLine[6][12:14]), int(splitLine[6][15:17]), int(splitLine[6][18:20]), 0, pytz.timezone('Etc/GMT+10'))
           # tz = pytz.utc
            date = date.astimezone(pytz.timezone('UTC')) 
            dateSeconds = time.mktime(date.timetuple())
            firstOf2008Seconds = time.mktime(firstOf2008.timetuple())

            minutesSince = (dateSeconds - firstOf2008Seconds) / 60
            dates.append(minutesSince)        
        
        dark_c = 0.066
        scale_c = 10
        dark_t = 0.062
        scale_t = 5

        for i in range(len(chlorophylls)):
            if chlorophylls[i] != -999.0:
                chlorophylls[i] = ((chlorophylls[i] - dark_c)*scale_c)/1000000
        for i in range(len(turbidities)):
            if turbidities[i] != -999.0:
                turbidities[i] = (turbidities[i] - dark_t)*scale_t


       # temperature = temperature.reshape(18,1,1,1)

        toWriteDataset.variables["lat"][:] = 21.2682
        toWriteDataset.variables["lon"][:] = -157.7767
        toWriteDataset.variables["z"][:] = -1.5
        toWriteDataset.variables["temp"][:] = temperatures #, 1, 6.8054, 158.1129
        toWriteDataset.variables["cond"][:] = conductivities#, 1, 6.8054, 158.1129
        toWriteDataset.variables["pres"][:] = pressures
        toWriteDataset.variables["flor"][:] =  chlorophylls#, 1, 6.8054, 158.1129  
        toWriteDataset.variables["turb"][:] =  turbidities#, 1, 6.8054, 158.1129
        toWriteDataset.variables["salt"][:] =  salinities#, 1, 6.8054, 158.1129
        toWriteDataset.variables["time"][:] = dates
        
        
        
        
        
        toWriteDataset.close()
       # print("closed")
       # open(f)


# Initialize the netCDF file with the correct dimensions and all that. No data to be added just yet
# Input: date
# Output: netCDF file pointer
def initNetCDF(dsetFilename):

    fillvalue = -999.0

    dataSet = Dataset(dsetFilename, "w", format="NETCDF4")
    dataSet.createDimension("time", None)
    dataSet.createDimension("z", 1)
    dataSet.createDimension("lat", 1)
    dataSet.createDimension("lon", 1)

    dataSet.title = "Nearshore sensor PP05, Waialae Beach Park, Maunalua Bay, Oahu, Hawaii"

    timeVar = dataSet.createVariable("time", "f4", ("time"),fill_value=fillvalue)
    timeVar.long_name="Time"
    timeVar.standard_name="time"
    timeVar.short_name="time"
    timeVar.axis="T"
    timeVar.units="minutes since 2008-01-01 00:00:00"
  #  timeVar._FillValue = fillvalue

    zVar = dataSet.createVariable("z", "f4", ("z"),fill_value=fillvalue)
    zVar.long_name="depth below mean sea level"
    zVar.standard_name="depth"
    zVar.short_name="depth"
    zVar.axis="z"
    zVar.units="meters"
   # zVar._FillValue = fillvalue

    latVar = dataSet.createVariable("lat","f4",("lat"),fill_value=fillvalue)
    latVar.long_name="Latitude"
    latVar.standard_name="latitude"
    latVar.short_name="lat"
    latVar.axis="Y"
    latVar.units="degrees_north"
   # latVar._FillValue = fillvalue

    lonVar = dataSet.createVariable("lon","f4",("lon"),fill_value=fillvalue)
    lonVar.long_name="Longitude"
    lonVar.standard_name="longitude"
    lonVar.short_name="lon"
    lonVar.axis="X"
    lonVar.units="degrees_east"
   # lonVar._FillValue = fillvalue
    
    tempVar = dataSet.createVariable("temp", "f4", ("time","z","lat","lon"),fill_value=fillvalue)
    tempVar.long_name="Temperature"
    tempVar.standard_name="sea_water_temperature"
    tempVar.short_name="temp"
    tempVar.units="Celsius"
   # tempVar._FillValue = fillvalue
    tempVar.valid_range=10.0,35.0

    conductivityVar = dataSet.createVariable("cond", "f4", ("time","z","lat","lon"),fill_value=fillvalue)
    conductivityVar.long_name="Conductivity"
    conductivityVar.standard_name="sea_water_electrical_conductivity"
    conductivityVar.short_name="cond"
    conductivityVar.units="S m-1"
  #  conductivityVar._FillValue = fillvalue
    conductivityVar.valid_range=0.0,50.0

    pressureVar = dataSet.createVariable("pres", "f4", ("time","z","lat","lon"),fill_value=fillvalue)
    pressureVar.long_name="Pressure"
    pressureVar.standard_name="sea_water_pressure"
    pressureVar.short_name="pres"
    pressureVar.units="dbar"
  #  pressureVar._FillValue = fillvalue
    pressureVar.valid_range=0,100


    chlorophyllVar = dataSet.createVariable("flor", "f4", ("time","z","lat","lon"),fill_value=fillvalue)
    chlorophyllVar.long_name="Chlorophyll"
    chlorophyllVar.standard_name="chlorophyll_concentration_in_sea_water"
    chlorophyllVar.short_name="flor"
    chlorophyllVar.units = "kg m-3"
 #   chlorophyllVar._FillValue = fillvalue
    chlorophyllVar.valid_range=0.0,10.0

    turbidityVar = dataSet.createVariable("turb", "f4", ("time","z","lat","lon"),fill_value=fillvalue)
    turbidityVar.long_name="Turbidity"
    turbidityVar.standard_name="turbidity_of_sea_water"
    turbidityVar.short_name="turb"
    turbidityVar.units="ntu"
 #   turbidityVar._FillValue = fillvalue
    turbidityVar.valid_range=0.0,10.0

    salinityVar = dataSet.createVariable("salt", "f4", ("time","z","lat","lon"),fill_value=fillvalue)
    salinityVar.long_name="Salinity"
    salinityVar.standard_name="sea_water_salinity"
    salinityVar.short_name="salt"
    salinityVar.units="1e-3"
  #  salinityVar._FillValue = fillvalue
    salinityVar.valid_range = 10.0, 40.0

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




