import io
import datetime

import numpy as np
import matplotlib.dates as mdates

from requests import get
from urllib.request import urlopen
from scipy.interpolate import griddata

from bokeh.plotting import figure, output_file, save, show
from bokeh.embed import components
from bokeh.models import RangeTool, Range1d, CustomJSTickFormatter
from bokeh.layouts import column
from bokeh import palettes
from bokeh import __version__ as bokeh_version
from bokeh.models import DatetimeTicker  # , DatetimeTickFormatter

# ----------------------------------------------------------------------------------------

# save only plot to single html file?
SAVE_STATIC = False

# only show the plot, don't save it?
SHOW = False

# if the specified palette is not available, it will fall back to 'Spectral8'
COLOR_PALETTE = "RdBu"

# ----------------------------------------------------------------------------------------

template_html = f"""
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Document</title>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Document</title>
    <script type="text/javascript" src="https://cdn.bokeh.org/bokeh/release/bokeh-{bokeh_version}.min.js"></script>
    <script type="text/javascript" src="https://cdn.bokeh.org/bokeh/release/bokeh-widgets-{bokeh_version}.min.js"></script>
    <script type="text/javascript">
        Bokeh.set_log_level("info");
    </script>
  </head>
  <body>
    {{{{TEMPLATE_DIV}}}}
  </body>
  <script src="plot.js"></script>
</html>
"""

# ----------------------------------------------------------------------------------------


def get_palette(palette_name: str, n_colors: int = 9):
    """
    Get color palette by name.
    Common number of colors are [9, 10, 11, 12, 256].
    If n is not available, interp_palette is used to generate one.
    """

    try:
        palette_full_name = f"{palette_name}{n_colors}"
        return getattr(palettes, palette_full_name)
    except AttributeError:
        pass

    try:
        palette = getattr(palettes, palette_name)
    except AttributeError:
        print(f"Palette {palette_name} not found. Using Spectral8 instead.")
        return palettes.Spectral8

    return palettes.interp_palette(palette[max(palette.keys())], n_colors)


def download(url, file_name):
    # open in binary mode
    with open(file_name, "wb") as file:
        # get request
        response = get(url)
        # write to file
        file.write(response.content)


def read_singapore(nmonth, nyear):
    headerlines = []
    ye = []
    dats = []
    count = 0
    date = []
    url = "https://www.atmohub.kit.edu/data/singapore.dat"
    # download(url,"Data/singapore.dat")
    with urlopen(url) as file:
        for i in range(3):
            headerlines.append(file.readline().decode().strip())
        for year in range(1987, nyear + 1):
            if year == nyear:
                data = np.zeros([nmonth, 15]) * np.nan
            else:
                data = np.zeros([12, 15]) * np.nan
            ye.append(file.readline().strip())
            file.readline()
            if year < 1997:
                data = np.zeros([12, 15]) * np.nan
                for i in range(14):
                    cols = file.readline()
                    cols = cols.strip().split()
                    for j in range(1, 13):
                        if i == 1:
                            date.append(datetime.datetime(year, j, 1))
                        data[j - 1, i] = float(cols[j])
            else:
                for i in range(15):
                    cols = file.readline()
                    cols = cols.strip().split()
                    for j in range(1, 13):
                        if j < nmonth + 1 or year < nyear:
                            if i == 1:
                                date.append(datetime.datetime(year, j, 1))
                            data[j - 1, i] = float(cols[j])
            dats.extend([list(i) for i in data])
            file.readline()
            count = count + 1
    pressure = [100, 90, 80, 70, 60, 50, 45, 40, 35, 30, 25, 20, 15, 12, 10]
    altitude = -7 * np.log(np.array(pressure) / 1013.25)
    fds = list(mdates.date2num(date))
    fds = np.array(fds)
    return np.array(dats).T[::-1], fds, pressure, altitude


def read_qbo():
    url = "https://www.atmohub.kit.edu/data/qbo.dat"

    with urlopen(url) as response:
        content = response.read().decode("utf-8")

    data = np.genfromtxt(
        io.StringIO(content),
        skip_header=9,
        dtype=[
            "S6",
            "S4",
            "i4",
            "i4",
            "i4",
            "i4",
            "i4",
            "i4",
            "i4",
            "i4",
            "i4",
            "i4",
            "i4",
            "i4",
            "i4",
            "i4",
        ],
        names=[
            "station",
            "date",
            "p70",
            "n70",
            "p50",
            "n50",
            "p40",
            "n40",
            "p30",
            "n30",
            "p20",
            "n20",
            "p15",
            "n15",
            "p10",
            "n10",
        ],
        delimiter=[6, 4, 6, 2, 5, 2, 5, 2, 5, 2, 5, 2, 5, 2, 5, 2],
        filling_values=-999999,
        missing_values=" ",
    )

    # url = "https://www.atmohub.kit.edu/data/qbo.dat"
    ##download(url,"singapore_winds_1953-2017_qbo.dat")
    # data = np.genfromtxt('/Users/tobias/Git/QBOSingapore/srv/qbo.dat',skip_header=9,dtype=['S6','S4','i4','i1','i4','i1','i4','i1','i4','i1','i4','i1','i4','i1','i4','i1'],names=['station','date','p70','n70','p50','n50','p40','n40','p30','n30','p20','n20','p15','n15','p10','n10'],delimiter=[6,4,6,2,5,2,5,2,5,2,5,2,5,2,5,2],filling_values=-999999,missing_values=' ')#
    date = []
    dlen = 0
    for i in range(len(data)):
        # print(data['date'][i])
        if data["date"][i]:
            dlen = dlen + 1
            if int(data["date"][i]) > 5000:
                date.append(
                    datetime.datetime.strptime(
                        "19" + (data["date"][i]).decode("UTF-8"), "%Y%m"
                    )
                )
            else:
                date.append(
                    datetime.datetime.strptime(
                        "20" + (data["date"][i]).decode("UTF-8"), "%Y%m"
                    )
                )

    fds = mdates.date2num(date)
    #
    up = np.array(
        list(
            [
                data["p70"],
                data["p50"],
                data["p40"],
                data["p30"],
                data["p20"],
                data["p15"],
                data["p10"],
            ]
        )
    )

    pressure = [70, 50, 40, 30, 20, 15, 10]
    altitude = -7 * np.log(np.array(pressure) / 1013.25)
    #    print(up)
    return up, fds, pressure, altitude


def main():
    up1, fds1, pressure1, altitude1 = read_qbo()
    tnmoth = np.shape(up1)[1]  # total number of months
    up = np.zeros([15, tnmoth]) * np.nan
    nmonth = np.shape(up1)[1] % 12  # number of months above a whole year
    nyear = mdates.num2date(fds1[-1]).year
    if nmonth == 0:
        nyear += 1
    print(nmonth, nyear)
    up2, fds2, pressure, altitude = read_singapore(nmonth, nyear)
    fds = fds1
    print(fds[-fds2.size], fds[-fds2.size + 1])
    print(fds2[0])
    print(np.size(fds1), np.size(fds2))
    fds[-fds2.size :] = fds2
    fds = np.array(fds)
    print(np.size(fds), fds[-1], fds1[-1], fds2[-1])
    print(mdates.num2date(fds2[-1]))
    up1 = up1 * 1.0
    up1[up1 < -10000] = np.nan
    up[3, :] = up1[0, :]
    up[5, :] = up1[1, :]
    up[7, :] = up1[2, :]
    up[9, :] = up1[3, :]
    up[11, :] = up1[4,]
    up[12, :] = up1[5, :]
    up[14, :] = up1[6, :]
    up[-up2.shape[0] :, -up2.shape[1] :] = up2
    ind = ~np.isnan(up)
    time, press = np.meshgrid(fds, pressure)
    uu = np.array(up[ind]) * 0.1
    time = time[ind]
    press = press[ind]

    # u = griddata(time,press,uu,fds,pressure,interp='linear')
    # print(np.shape(time), np.shape(press), np.shape(uu))
    # print(np.shape(fds), np.shape(pressure))
    points1 = np.array([[time[i], press[i]] for i in range(len(time))])
    # print(points1.shape)
    X, Y = np.meshgrid(fds, pressure)
    u = griddata(points1, uu, (X, Y), method="linear")
    # nrows = 7

    contour_levels = list(range(-40, 45, 5))

    deltat = int((np.nanmax(fds) - fds[0]) / 1) + 1

    by0 = fds[0]
    by1 = fds[0] + deltat

    axt = fds[np.nanargmin(abs(fds - by0)) : 1 + np.nanargmin(abs(fds - by1))]

    axz = u[:, np.nanargmin(abs(fds - by0)) : 1 + np.nanargmin(abs(fds - by1))]

    # axt = [t.strftime("%m/%Y") for t in mdates.num2date(axt)]

    p = figure(
        width=1200,
        height=500,
        y_axis_type="log",
        y_range=(max(pressure), min(pressure)),  # type: ignore
        x_range=(axt[0], axt[100]),  # type: ignore
        y_axis_label="Pressure [hPa]",
        # title="Quasi-Biennial-Oscillation (QBO)"
    )
    p.title.text_font_size = "25px"  # type: ignore
    p.title.align = "center"  # type: ignore

    contour_renderer = p.contour(
        axt + 7 * 30,
        pressure,
        axz,
        contour_levels,
        fill_color=get_palette(COLOR_PALETTE, len(contour_levels)),
        line_color="black",
    )

    select = figure(
        width=1200,
        height=200,
        y_axis_type="log",
        tools="",
        toolbar_location=None,
        y_range=p.y_range,
        x_axis_label="Time [year]",
    )

    range_tool = RangeTool(x_range=p.x_range)
    range_tool.overlay.fill_color = "red"
    range_tool.overlay.fill_alpha = 0.4

    select.contour(
        axt + 5 * 30 * 12 + 4 * 30 + 15,
        pressure,
        axz,
        contour_levels,
        line_alpha=0.1,
        fill_color=get_palette(COLOR_PALETTE, len(contour_levels)),
        line_color="black",
    )
    select.add_tools(range_tool)

    # # Set yearly intervals for the top plot
    # p.xaxis.ticker = DatetimeTicker(desired_num_ticks=22)  # Adjust for more control over spacing

    # p.xaxis[0].formatter = CustomJSTickFormatter(
    #     code="""
    # const milliseconds = tick * 24 * 60 * 60 * 1000;

    # // Create a Date object using the calculated milliseconds
    # const date = new Date(milliseconds);

    # // Extract the month and year from the Date object
    # const month = (date.getUTCMonth() + 1).toString().padStart(2, '0'); // Month is 0-based
    # const year = date.getUTCFullYear();

    # // Format the result as MM/YYYY
    # return `${month}/${year}`;
    # """
    # )
    # # Set 10-year intervals for the bottom plot
    # select.xaxis.ticker = DatetimeTicker(desired_num_ticks=15)  # Larger gap for decades

    # select.xaxis[0].formatter = CustomJSTickFormatter(
    #     code="""
    # const milliseconds = tick * 24 * 60 * 60 * 1000;

    # // Create a Date object using the calculated milliseconds
    # const date = new Date(milliseconds);

    # // Extract the month and year from the Date object
    # const month = (date.getUTCMonth() + 1).toString().padStart(2, '0'); // Month is 0-based
    # const year = date.getUTCFullYear();

    # // Format the result as MM/YYYY
    # return `${month}/${year}`;
    # """
    # )

    # # Top plot: yearly labels starting from January 1st
    # p.xaxis.formatter = CustomJSTickFormatter(
    # code="""
    # const date = new Date(tick * 24 * 60 * 60 * 1000);
    # date.setUTCMonth(0);
    # date.setUTCDate(1);
    # return `${date.getUTCFullYear()}`;
    # """
    # )

    # # Bottom plot: 10-year intervals with clean formatting
    # select.xaxis.formatter = CustomJSTickFormatter(
    # code="""
    # const date = new Date(tick * 24 * 60 * 60 * 1000);
    # const year = date.getUTCFullYear();
    # if (year % 10 === 0) {  // Show only on 10-year intervals
    #     return `${year}`;
    # } else {
    #     return "";
    # }
    # """
    # )

    # Top Plot (Align the labels correctly)

    p.xaxis.formatter = CustomJSTickFormatter(  # type: ignore
        code="""
        const date = new Date(tick * 24 * 60 * 60 * 1000);  // Convert from date2num to milliseconds
        const year = date.getUTCFullYear();  // Extract the year
        return `${year}`;  // Return the year
        """
    )

    # Bottom Plot (10-Year Interval Labels)
    select.xaxis.formatter = CustomJSTickFormatter(  # type: ignore
        code="""
        const date = new Date(tick * 24 * 60 * 60 * 1000);  // Convert from date2num to milliseconds
        const year = date.getUTCFullYear();  // Extract the year
        const roundedYear = Math.floor(year / 10) * 10;  // Round to nearest decade
        return `${roundedYear}`;  // Return the decade
        """
    )

    p.xaxis.ticker.num_minor_ticks = 12  # type: ignore
    select.xaxis.ticker.num_minor_ticks = 10  # type: ignore
    # select.xaxis.formatter = CustomJSTickFormatter(
    #    code="""
    #    const date = new Date(tick * 24 * 60 * 60 * 1000);  // Convert from date2num to milliseconds
    #    const year = date.getUTCFullYear();  // Extract the year
    #    // Round to the nearest decade
    #    const roundedYear = Math.floor(year / 10) * 10;
    #    // Shift the first label by half a decade (5 years)
    #    const shiftedYear = roundedYear + 5;
    #    return `${shiftedYear}`;  // Return the shifted year
    #    """
    # )

    # Adjust ticker for bottom plot to show yearly ticks (desired_num_ticks adjusted)
    select.xaxis.ticker = DatetimeTicker(  # type: ignore
        desired_num_ticks=12
    )  # Adjust to suit your range

    select.yaxis.major_label_text_font_size = "0pt"  # type: ignore

    select.yaxis.major_tick_line_color = (  # type: ignore
        None  # turn off y-axis major ticks
    )
    select.yaxis.minor_tick_line_color = (  # type: ignore
        None  # turn off y-axis minor ticks
    )

    p.axis.axis_label_text_font_size = "20px"  # type: ignore
    select.axis.axis_label_text_font_size = "20px"  # type: ignore
    p.axis.major_label_text_font_size = "15px"  # type: ignore
    select.axis.major_label_text_font_size = "15px"  # type: ignore
    select.yaxis.major_label_text_font_size = "0px"  # type: ignore

    altitude = -7 * np.log(np.array(pressure) / 1013.25)

    p.extra_y_ranges["altitude"] = Range1d(min(altitude), max(altitude))
    # p.contour(axt, pressure, axz, contour_levels, fill_color=Sunset8, line_color="black", y_range_name='altitude')

    colorbar = contour_renderer.construct_color_bar()
    colorbar.title = "East to West (-/+) monthly mean zonal winds [m/s]"
    colorbar.title_text_font_size = "15px"
    p.add_layout(colorbar, "below")

    select.title = "Move/resize slider by dragging"
    select.title.text_font_size = "15px"  # type: ignore

    if SHOW:
        show(column(p, select))
        # show(p)
        return None, None

    if SAVE_STATIC:
        output_file(filename="plot_only.html", title="Static HTML file")
        save(p)
        return None, None

    return components(column(p, select))


if __name__ == "__main__":

    script, div = main()

    if all(x is not None for x in (script, div)):
        # print(div)
        updated_html = template_html.replace("{{TEMPLATE_DIV}}", div)  # type: ignore

        with open("plot.js", "w") as file:
            file.write(
                script.replace('<script type="text/javascript">', "").replace(  # type: ignore
                    "</script>", ""
                )
            )

        with open("plot.html", "w") as file:
            file.write(updated_html)
