function qbo(args)

'reinit'

'open ../qbo_highres.ctl'

_pas = '100 70 50 30 20 10'
_pas = '100 70 50 40 30 20 15 10'
_kms = '18 22 26 30'
_kms = '17 20 23 26 29 32'
cthick = 3

* *****************************************************
* Anzahl der Abbildungen auf der Seite
* nyimage   - Anzahl der übereinander stehenden  Bilder
* nximage   - Anzahl der nebeneinander stehenden Bilder
* *****************************************************
nyimage = 2
nximage = 1

ypr    = 4
eyear  = 2014
syear1 = eyear - nyimage*ypr
eyear1 = syear1 + ypr
syear2 = eyear1+1
eyear2 = syear2 + ypr

say 'syear1 = 'syear1' eyear1 = 'eyear1
say 'syear2 = 'syear2' eyear2 = 'eyear2

ofile = 'qbo-'syear1'-'eyear2
'enable print 'ofile'.hc'

* ********************************
*  gxinfo 
* ********************************
'q gxinfo'
x2 = sublin(result,2)
x2 = subwrd(x2,4)
x1 = 0
y2 = sublin(result,2)
y2 = subwrd(y2,6)
y1 = 0

*
* ***********************************************************
* Abstände: xbr  - rechter Rand
*           xbl  - linker Rand
*           ybb  - unterer Rand
*           ybt  - oberer Rand
*           xs   - horizontaler Abstand zwischen zwei Bidern
*           ys   - vertikaler Abstand zwischen zwei Bidern
* ***********************************************************
xbr = 1.0
xbl = 0.8
ybb = 0.5
ybt = 1.0
ys = 1.0
xs = 0.0
*
* *************************************
* Größe der Zeichenfläche insgesamt:
* xp   -  Breite
* yp   -  Höhe
* *************************************
xp = (x2-x1) - xbr - xbl
yp = (y2-y1) - ybb - ybt 
*
* *****************************************
* Höhe und Breite der einzelnen Abbildungen
* *****************************************
height = (yp - (nyimage-1)*ys) /nyimage
width = height
width = (xp - (nximage-1)*xs) /nximage
xladd=on
say 'height:'height
say 'width:'width

'set zlog on'
'set xlopts 1 5 0.12'
'set ylopts 1 5 0.14'
'set vpage 'x1' 'x2' 'y1' 'y2
'set ylevs '_pas
*****************************************************
nx = 1
ny = 1
yy1 = ybb + (height+ys)*(ny-1)
yy2 = yy1 + height 
xx1 = xbl + (width+xs)*(nx-1)
xx2 = xx1 + width

'set parea 'xx1' 'xx2' 'yy1' 'yy2
'set grads off'
'set time 01jan'syear2' 01dec'eyear2
'set gxout shaded'
'set clevs 0'
'set ccols 0 11'
'd u'
'set gxout contour'
'set cint 10'
'set ccolor 1'
'set cthick 'cthick
'd u'
km(xx1' 'xx2' 'yy1' 'yy2)
*****************************************************
nx = 1
ny = 2
yy1 = ybb + (height+ys)*(ny-1)
yy2 = yy1 + height 
xx1 = xbl + (width+xs)*(nx-1)
xx2 = xx1 + width

'set parea 'xx1' 'xx2' 'yy1' 'yy2
'set grads off'
'set time 01jan'syear1' 01dec'eyear1
'set gxout shaded'
'set clevs 0'
'set ccols 0 11'
'd u'
'set gxout contour'
'set cint 10'
'set ccolor 1'
'set cthick 'cthick
'd u'
km(xx1' 'xx2' 'yy1' 'yy2)

'print 'ofile'.hc'
'disable print 'ofile'.hc'
'!gxeps -i -c 'ofile'.hc -o eps/'ofile'.eps'
'!rm 'ofile'.hc'
say 'eps/'ofile'.eps'


*********************************
*********************************
function km(args)

xx1 = subwrd(args,1)
xx2 = subwrd(args,2)
yy1 = subwrd(args,3)
yy2 = subwrd(args,4)

input2 = qbo_highres_km
'open ../'input2'.ctl'
'q file 2'
ckopen = sublin(result,1)
ckopen = subwrd(ckopen,1)
if (ckopen != 'File')
   say 'File 2: 'input2' is not open!'
   return
endif

* *************************
* km Skala
* *************************
'set dfile 2'
ls=0.003
'set parea 'xx2' 'xx2+ls' 'yy1' 'yy2
'set cthick 1'
'set gxout contour'
'set grads off'
'set ylpos 0 r'
'set xlab off'
'set zlog off'
'set z 1 15'
'set ylevs  '_kms
'set ccolor 1'
'd u'
'set string 1 tc 5'  
'set strsiz 0.13'
'draw string 'xx2+0.3' 'yy2-0.1' km'
'draw string 'xx1-0.3' 'yy2-0.3' hPa'
'set ylpos 0 l'
'set zlog on'
'set dfile 1'
'close 2'
'set z 1 15'
'set ylevs '_pas
'set xlab on'
return


