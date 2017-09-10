import csv
import numpy as np
import re
#import matplotlib.pyplot as plt
#import scipy.fftpack


osReader = csv.reader(open("outputSet.csv","rb"))
numCol = len(next(osReader))
currMax = [0.0] * numCol # List of maximums for each column
prevMax = [0.0] * numCol # List of second maximums for each column
factorMax = [0.0] * numCol # List of maximums multiplied by a constant
currMin = [0.0] * numCol # List of minimums for each column
maxRatio = [0.0] * numCol # List of max/2nd max values
sums = [0.0] * numCol # Used for calculating avg
avg = [0.0] * numCol # List of average values (mean) for each columm
count = [0.0] * numCol # You know

rowIndex=0
for l in (osReader):
        colIndex=0
        for val in l:
                try:
                        newVal = np.absolute(complex(val))
                        if float(newVal) > currMax[colIndex]:
                                prevMax[colIndex] = currMax[colIndex]
                                currMax[colIndex] = float(newVal)
                        if float(newVal) < currMin[colIndex]:
                                currMin[colIndex] = float(newVal)
                        sums[colIndex] += newVal
                except:
                        a=0
                colIndex = colIndex + 1
        rowIndex = rowIndex + 1

avg = [ i / 3197.0 for i in sums ]

for i in range (1 , len(currMax)):
        if float(prevMax[i]) != 0:
                maxRatio[i] = float(currMax[i])/float(prevMax[i])
        else:
                maxRatio[i] = 'null'

factorMax = [ i * 0.36787 for i in currMax ] # change the constant here

for i in range (1, len(currMin)):
        currMin[i] = currMax[i]

osReader = csv.reader(open("outputSet.csv","rb"))

for l in (osReader):
        colIndex=0
        for val in l:
                try:
                        newVal = np.absolute(complex(val))
                        if newVal > factorMax[colIndex]:
                                count[colIndex] = count[colIndex] + 1
                        if newVal < currMin[colIndex]:
                                currMin[colIndex] = float(newVal)


                except:
                        a=0
                colIndex = colIndex + 1