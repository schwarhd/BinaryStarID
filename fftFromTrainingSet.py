import csv
import numpy as np
import matplotlib.pyplot as plt
import scipy.fftpack
import pandas as pd

c = open('outputSet.csv', 'w')
c.write('')
c.close

c = open('outputSet.csv', 'a')



with open("miniset.csv", "rb") as f:
    reader = csv.reader(f)
    rownum = 0
    for i, line in enumerate(reader):
        if i > 0:
            l = line[0]
            r = line[1]
            data = []
            for val in line:
                try:
                    # if val != line[0]:
                    data.append(float(val))
                    #print val
                except:
                    print "bad value - " + line[q]
            #print len(data)
            # %matplotlib inline
            # Number of samplepoints
            N = len(data)
            # sample spacing
            T = 1.0 / len(data)
            # x = np.linspace(0.0, N*T, N)
            # y = np.sin(50.0 * 2.0*np.pi*x) + 0.5*np.sin(80.0 * 2.0*np.pi*x)
            y = data
            yf = scipy.fftpack.fft(y)
            yfs = (2.0 / N * np.abs(yf[:N // 2]))
            # print yf
            xf = np.linspace(0.0, 1.0/(2.0*T), N/2)
            p = 0
            for val in yfs:
                if p >= 0:
                    r += ','
                r += str(val)
                p += 1
            r += "\n"
            l += ','
            c.write(l)
            c.write(r)
            # if len(tokens) > 0:
            # cur.execute(s,(tokens))
            # print yfs[3], yf[3]

            # print len(yf)
            # if rownum % 100 == 0:
            # print rownum
            # rownum += 1
    c.close()
