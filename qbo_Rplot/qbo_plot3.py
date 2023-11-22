import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from filled_contour3 import filled_contour3

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

# 100 hPa for the period 1957 - 1996
data2 = pd.read_table(fname2)
qbo2 = data2.iloc[:, [2, 3, 13]] / 10
qbo2.columns = ["YY", "MM", "100hPa"]
qbo2.loc[qbo2["YY"] > 50, "YY"] += 1900
qbo2.loc[qbo2["YY"] <= 50, "YY"] += 2000

# 100 hPa since 1997
data3 = pd.read_table(fname3)
qbo3 = data3.iloc[:, :3]
qbo3.columns = ["YY", "MM", "100hPa"]

# From 1987
data4 = pd.read_table(fname4)
qbo4 = data4.iloc[:, :16]

# Create an empty DataFrame with NaN values
qbo4_neu = pd.DataFrame(index=range(419), columns=range(16))
qbo4_neu[:] = np.nan

# Perform the operations on the DataFrame
qbo4_neu.iloc[:, 2] = np.concatenate((qbo1[:408, 3], qbo4_neu.iloc[:, 15], np.full(73, np.nan)))  # 70 hPa
qbo4_neu.iloc[:, 3] = np.concatenate((np.full(408, np.nan), qbo4_neu.iloc[:, 14], np.full(73, np.nan)))  # 65 hPa
qbo4_neu.iloc[:, 4] = np.concatenate((qbo1[:408, 4], qbo4_neu.iloc[:, 13], np.full(73, np.nan)))  # 60 hPa
qbo4_neu.iloc[:, 5] = np.concatenate((np.full(408, np.nan), qbo4_neu.iloc[:, 12], np.full(73, np.nan)))  # 55 hPa
qbo4_neu.iloc[:, 6] = np.concatenate((qbo1[:408, 5], qbo4_neu.iloc[:, 11], np.full(73, np.nan)))  # 50 hPa
qbo4_neu.iloc[:, 7] = np.concatenate((np.full(408, np.nan), qbo4_neu.iloc[:, 10], np.full(73, np.nan)))  # 45 hPa
qbo4_neu.iloc[:, 8] = np.concatenate((qbo1[:408, 6], qbo4_neu.iloc[:, 9], np.full(73, np.nan)))  # 40 hPa
qbo4_neu.iloc[:, 9] = np.concatenate((np.full(408, np.nan), qbo4_neu.iloc[:, 8], np.full(73, np.nan)))  # 35 hPa
qbo4_neu.iloc[:, 10] = np.concatenate((qbo1[:408, 7], qbo4_neu.iloc[:, 7], np.full(73, np.nan)))  # 30 hPa
qbo4_neu.iloc[:, 11] = np.concatenate((np.full(408, np.nan), qbo4_neu.iloc[:, 6], np.full(73, np.nan)))  # 25 hPa
qbo4_neu.iloc[:, 12] = np.concatenate((qbo1[:408, 8], qbo4_neu.iloc[:, 5], np.full(73, np.nan)))  # 20 hPa
qbo4_neu.iloc[:, 13] = np.concatenate((qbo1[:408, 9], qbo4_neu.iloc[:, 4], np.full(73, np.nan)))  # 15 hPa
qbo4_neu.iloc[:, 14] = np.concatenate((np.full(408, np.nan), qbo4_neu.iloc[:, 3], np.full(73, np.nan)))  # 12 hPa
qbo4_neu.iloc[:, 15] = np.concatenate((qbo1[:408, 10], qbo4_neu.iloc[:, 2], np.full(73, np.nan)))  # 10 hPa

for j in range(408):
    qbo4_neu.iloc[j, 0] = lev_qbo_sort[0, j]  # pressure levels
    qbo4_neu.iloc[j, 1] = -np.log(lev_qbo_sort[0, j])  # altitude levels

# Generate the plot
label_array = np.array([
    ["1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989"],
    ["1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998"],
    ["1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007"],
    ["2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"],
    ["2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"],
    ["2026", "2027", "2028", "2029", "2030", "2031", "2032", "2033", "2034"],
    ["2035", "2036", "2037", "2038", "2039", "2040", "2041", "2042", "2043"],
    ["2044", "2045", "2046", "2047", "2048", "2049", "2050", "2051", "2052"]
])

plt.figure(figsize=(5.8, 8.3))
for i in range(8):
    plt.subplot(8, 1, i+1)
    plt.contourf(np.arange(1, 109), -qbo4_neu.iloc[:, 1], qbo4_neu.iloc[i*52:(i+1)*52, 2:17], colors=("grey", "white"))
    plt.xticks(np.arange(6, 103, 12), label_array[i], fontsize=8, rotation=90)
    plt.xlim(0, 108)
    plt.ylim(-np.log(100), -np.log(10))
    plt.yticks(-np.log([100, 70, 50, 30, 20, 10]), [100, 70, 50, 30, 20, 10])
    plt.axhline(y=-np.log([75, 42, 25, 14]), color="dimgrey", linestyle="dashed", linewidth=0.6)
    plt.axvline(x=12.5, color="dimgrey", linestyle="dashed", linewidth=0.6)
    plt.colorbar(label="Wind [m/s]")
    plt.contour(np.arange(1, 109), -qbo4_neu.iloc[:, 1], qbo4_neu.iloc[i*52:(i+1)*52, 2:17], levels=[-30, -20, -10, 10, 20, 30], linewidths=0.5, colors="black")
    plt.contour(np.arange(1, 109), -qbo4_neu.iloc[:, 1], qbo4_neu.iloc[i*52:(i+1)*52, 2:17], levels=[0], linewidths=0.9, colors="black")
    plt.text(-5, -3.5, "hPa", fontsize=10, fontweight="bold", rotation=90, va="center", ha="center")
    plt.title("Winter winds at 30°N", fontsize=10, fontweight="bold")

plt.tight_layout()
plt.show()
