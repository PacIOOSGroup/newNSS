import os
import subprocess
import glob



def main():
    
    for i in range (2017,2019,1):
        for j in range (1, 13, 1):
            for k in range(1, 32, 1):
                toCat = str(i).zfill(2) + str(j).zfill(2) + str(k).zfill(2)
                fileName = "pp3data/pp3_palmyra_" + toCat + "*"
                print(fileName) 
                outFile = "pp3data/byDayUTC/" + toCat + ".txt"
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



