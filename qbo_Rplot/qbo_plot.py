import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from filled.contour3 import filled_contour3

fname = '../QBO/qbo_data/qbo.dat'       
fname2 = '../data/shea/qbo_u'						
fname3 = '../data/qbo_100hPa.dat'       
fname4 = '../data/qbo.highres.dat'      

line_n = 1
with open(fname) as f:
    for i, line in enumerate(f):
        if len(line.strip()) > 0:
            line_n += 1

qbo1 = np.empty((line_n, 10))
qbo1[:] = np.nan
qbo1 = pd.DataFrame(qbo1, columns=["station", "YY", "MM", "70hPa", "50hPa", "40hPa", "30hPa", "20hPa", "15hPa", "10hPa"])

with open(fname) as f:
    for i, line in enumerate(f):
        if len(line.strip()) > 0:
            txt = line.strip()
            station = int(txt[0:5])
            qbo1.iloc[i-9, 0] = station
            year = int(txt[6:8])
            if year > 50:
                year += 1900
            else:
                year += 2000
            qbo1.iloc[i-9, 1] = year
            mon = int(txt[8:10])
            qbo1.iloc[i-9, 2] = mon
            p70 = int(txt[12:16])
            qbo1.iloc[i-9, 3] = p70/10
            p50 = int(txt[19:23])
            qbo1.iloc[i-9, 4] = p50/10
            p40 = int(txt[26:30])
            qbo1.iloc[i-9, 5] = p40/10
            p30 = int(txt[33:37])
            qbo1.iloc[i-9, 6] = p30/10
            p20 = int(txt[40:44])
            qbo1.iloc[i-9, 7] = p20/10
            p15 = int(txt[47:51])
            qbo1.iloc[i-9, 8] = p15/10
            p10 = int(txt[54:58])
            qbo1.iloc[i-9, 9] = p10/10

qbo1 = qbo1.dropna(how='all')

