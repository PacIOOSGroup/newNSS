import os
import subprocess
import glob



def main():
    
    for i in range (2015,2018,1):
        for j in range (1, 13, 1):
            for k in range(1, 32, 1):
                toCat = str(i).zfill(2) + str(j).zfill(2) + str(k).zfill(2)
                fileName = "pp2data/pp2_pohnpei_" + toCat + "*"
                print(fileName) 
                outFile = "pp2data/byDayUTC/" + toCat + ".txt"
                fileList = sorted(glob.glob(fileName))
                if not not fileList:
                    toWrite = open(outFile, 'a+')
                    for f in fileList:
                        currentFile = open(f, 'r')
                        toWrite.write(currentFile.readline())
                        currentFile.close()
                    toWrite.close()



                

if __name__ == "__main__":
    main()



