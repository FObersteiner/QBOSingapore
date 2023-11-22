#
# Copy the QBO figures from strat25 to the working directory.
# It is necessary to work with idl on strat25.
#
FIGS = /local/home/aktuell/data/qbo/idl/figs
WDIR = .

#all: cp pdf png gif jpg
all: pdf png gif jpg

all_aktuell: pdf png gif jpg
cp:
	cp -p ${FIGS}/qbo_wind.eps ${WDIR}
	cp -p ${FIGS}/qbo_wind.ps ${WDIR}
	cp -p ${FIGS}/qbo_wind_letter.ps ${WDIR}
pdf:
	epstopdf qbo_wind.eps
gif:
	convert qbo_wind.pdf qbo_wind.gif	
#png:
#	convert -density 150 qbo_wind.pdf -quality 100 -alpha off qbo_wind.png	
png:
	convert -density 150 qbo_wind.pdf -quality 100 qbo_wind.png
jpg:
	convert qbo_wind.pdf qbo_wind.jpg	

clean:
	rm qbo_wind.gif qbo_wind.jpg qbo_wind.pdf qbo_wind.png
