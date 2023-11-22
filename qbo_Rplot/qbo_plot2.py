import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def filled_contour3(x, y, z, xlim=None, ylim=None, zlim=None, levels=None, nlevels=20,
color_palette=plt.cm.colors, col=None, plot_title=None, plot_axes=None,
key_title=None, key_axes=None, asp=None, xaxs="i", yaxs="i", las=1, axes=True,
frame_plot=None, mar=None, **kwargs):

python
Copy code
if z is None:
    if x is not None:
        if isinstance(x, dict):
            z = x['z']
            y = x['y']
            x = x['x']
        else:
            z = x
            x = np.linspace(0, 1, num=z.shape[0])
    else:
        raise ValueError("no 'z' matrix specified")
elif isinstance(x, dict):
    y = x['y']
    x = x['x']

if np.any(np.diff(x) <= 0) or np.any(np.diff(y) <= 0):
    raise ValueError("increasing 'x' and 'y' values expected")

if xlim is None:
    xlim = (np.min(x), np.max(x))
if ylim is None:
    ylim = (np.min(y), np.max(y))
if zlim is None:
    zlim = (np.min(z), np.max(z))
if levels is None:
    levels = np.linspace(zlim[0], zlim[1], num=nlevels)
if col is None:
    col = color_palette(len(levels) + 1)

plt.contourf(x, y, z, levels=levels, colors=col, **kwargs)
plt.xlim(xlim)
plt.ylim(ylim)
plt.xlabel('x')
plt.ylabel('y')
plt.title(plot_title)
plt.colorbar()
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
qbo1.iloc[i - 9, 0] = station
year = int(txt[6:8])
if year > 50:
year += 1900
else:
year += 2000
qbo1.iloc[i - 9, 1] = year
mon = int(txt[8:10])
qbo1.iloc[i - 9, 2] = mon
p70 = int(txt[12:16])
qbo1.iloc[i - 9, 3] = p70 / 10
p50 = int(txt[19:23])
qbo1.iloc[i - 9, 4] = p50 / 10
p40 =.





import numpy as np
import pandas as pd

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
            station = int(''.join(list(txt[0:5])))
            qbo1.iloc[i-9, 0] = station
            year = int(''.join(list(txt[6:8])))
            if year > 50:
                year += 1900
            else:
                year += 2000
            qbo1.iloc[i-9, 1] = year
            mon = int(''.join(list(txt[8:10])))
            qbo1.iloc[i-9, 2] = mon
            p70 = int(''.join(list(txt[12:16])))
            qbo1.iloc[i-9, 3] = p70/10
            p50 = int(''.join(list(txt[19:23])))
            qbo1.iloc[i-9, 4] = p50/10
            p40 = int(''.join(list(txt[26:30])))
            qbo1.iloc[i-9, 5] = p40/10
            p30 = int(''.join(list(txt[33:37])))
            qbo1.iloc[i-9, 6] = p30/10
            p20 = int(''.join(list(txt[40:44])))
            qbo1.iloc[i-9, 7] = p20/10
            p15 = int(''.join(list(txt[47:51])))
            qbo1.iloc[i-9, 8] = p15/10
            p10 = int(''.join(list(txt[54:58])))
            qbo1.iloc[i-9, 9] = p10/10

qbo1 = qbo1.dropna(how='all')

# 100 hPa fuer den Zeitraum 1957 - 1996
data2 = pd.read_table(fname2)
qbo2 = data2.iloc[:, [2, 3, 13]] / 10
qbo2.columns = ["YY", "MM", "100hPa"]
qbo2.loc[qbo2["YY"] > 50, "YY"] += 1900
qbo2.loc[qbo2["YY"] <= 50, "YY"] += 2000

# 100 hPa seit 1997
data3 = pd.read_table(fname3)
qbo3 = data3.iloc[:, :3]
qbo3.columns = ["YY", "MM", "100hPa"]

# ab 1987
data4 = pd.read_table(fname4)
qbo4 = data4.iloc[:, :16]

qbo4_neu = np.empty((419, 16.





import numpy as np
import pandas as pd

fname = '../QBO/qbo_data/qbo.dat'
fname2 = '../data/shea/qbo_u'
fname3 = '../data/qbo_100hPa.dat'
fname4 = '../data/qbo.highres.dat'

def read_qbo_file(filename):
    with open(filename) as f:
        lines = f.readlines()
    line_n = len(lines) - 10

    qbo = np.empty((line_n, 10))
    qbo[:] = np.nan
    qbo = pd.DataFrame(qbo, columns=["station", "YY", "MM", "70hPa", "50hPa", "40hPa", "30hPa", "20hPa", "15hPa", "10hPa"])

    for i in range(10, len(lines)):
        line = lines[i].strip()
        if len(line) > 0:
            station = int(line[0:5])
            qbo.iloc[i - 9, 0] = station
            year = int(line[6:8])
            if year > 50:
                year += 1900
            else:
                year += 2000
            qbo.iloc[i - 9, 1] = year
            mon = int(line[8:10])
            qbo.iloc[i - 9, 2] = mon
            p70 = int(line[12:16])
            qbo.iloc[i - 9, 3] = p70 / 10
            p50 = int(line[19:23])
            qbo.iloc[i - 9, 4] = p50 / 10
            p40 = int(line[26:30])
            qbo.iloc[i - 9, 5] = p40 / 10
            p30 = int(line[33:37])
            qbo.iloc[i - 9, 6] = p30 / 10
            p20 = int(line[40:44])
            qbo.iloc[i - 9, 7] = p20 / 10
            p15 = int(line[47:51])
            qbo.iloc[i - 9, 8] = p15 / 10
            p10 = int(line[54:58])
            qbo.iloc[i - 9, 9] = p10 / 10

    qbo = qbo.dropna(how='all')
    return qbo

qbo1 = read_qbo_file(fname)

data2 = pd.read_table(fname2)
qbo2 = data2.iloc[:, [2, 3, 13]] / 10
qbo2.columns = ["YY", "MM", "100hPa"]
qbo2.loc[qbo2['YY'] > 50, 'YY'] += 1900
qbo2.loc[qbo2['YY'] <= 50, 'YY'] += 2000

data3 = pd.read_table(fname3)
qbo3 = data3.iloc[:, :3]
qbo3.columns = ["YY", "MM", "100hPa"]

data4 = pd.read_table(fname4)
qbo4 = data4.iloc[:, :16]

qbo4_neu = np.empty((419, 16))
qbo4_neu[:] = np.nan
qbo4_neu = pd.DataFrame(qbo4_neu, columns=["YY", "MM", "10hPa", "12hPa", "15hPa", "20hPa", "25hPa", "30hPa.
