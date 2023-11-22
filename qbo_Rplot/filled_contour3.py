import numpy as np
import matplotlib.pyplot as plt

def filled_contour3(x, y, z, xlim = None, ylim = None, zlim = None, 
                    levels = None, nlevels = 20, color_palette = plt.cm.colors, 
                    col = None, plot_title = None, plot_axes = None, 
                    key_title = None, key_axes = None, asp = None, xaxs = "i", 
                    yaxs = "i", las = 1, axes = True, frame_plot = None, mar = None, **kwargs):
    
    if z is None:
        if x is not None:
            if isinstance(x, dict):
                z = x['z']
                y = x['y']
                x = x['x']
            else:
                z = x
                x = np.linspace(0, 1, num = z.shape[0])
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
        levels = np.linspace(zlim[0], zlim[1], num = nlevels)
    if col is None:
        col = color_palette(len(levels) + 1)
    
    plt.contourf(x, y, z, levels = levels, colors = col, **kwargs)
    plt.xlim(xlim)
    plt.ylim(ylim)
    plt.xlabel('x')
    plt.ylabel('y')
    plt.title(plot_title)
    plt.colorbar()
