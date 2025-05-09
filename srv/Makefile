#
# Copy the QBO figu:res from strat25 to the working directory.
# It is necessary to work with idl on strat25.
#
FIGS = ../qbo_Rplot
WDIR = .
REPO = ../QBO/qbo_data/
DATFILES = ${REPO}/qbo.dat ${REPO}/singapore202*.dat ${REPO}/singapore.dat 


#all: cp png gif jpg tar zip
all: cp tar zip jpg

cp:
	cp -p ${FIGS}/qbo_wind.pdf ${WDIR}
gif:
	magick qbo_wind.pdf qbo_wind.gif	
#png:
#	magick -density 150 qbo_wind.pdf -quality 100 -alpha off  qbo_wind.png	
png:
	magick -density 150 qbo_wind.pdf -quality 100 qbo_wind.png	
#jpg:
#	magick -density 150 qbo_wind.pdf -quality 100 -alpha off  qbo_wind.jpg
#jpg:
	#magick -density 300 qbo_wind.pdf -quality 100 qbo_wind.jpg
jpg:
	pdftoppm -jpeg -r 300 qbo_wind.pdf qbo_wind.jpg
	mv -f qbo_wind.jpg-1.jpg qbo_wind.jpg
tar:
	tar -czf qbo_data_tgz.tgz $(DATFILES)

zip:
	rm qbo_data_zip.zip
	zip -r qbo_data_zip.zip $(DATFILES)

clean:
	rm qbo_wind.gif qbo_wind.jpg qbo_wind.png *~
