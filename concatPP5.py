import os
import subprocess
import glob



def main():
    
    for i in range (2018,2020,1):
        for j in range (1, 13, 1):
            for k in range(1, 32, 1):
                toCat = str(i).zfill(2) + str(j).zfill(2) + str(k).zfill(2)
                fileName = "pp5data/originalFiles/pp5_oahu_waialae_" + toCat + "*"
                print(fileName) 
                outFile = "pp5data/byDayUTC/" + toCat + ".txt"
                fileList = sorted(glob.glob(fileName))
                if not not fileList:
                    toWrite = open(outFile, 'a+')
                    for f in fileList:
                        currentFile = open(f, 'r')
                        for q in currentFile.readlines():
                            toWrite.write(q)
                        #toWrite.write(currentFile.readlines())
                        currentFile.close()
                    toWrite.close()



                

if __name__ == "__main__":
    main()



