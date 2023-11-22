function qbo(args)

'reinit'

rc    = gsfallow("on")

_syear = subwrd(args,1)
_eyear = subwrd(args,2)
_color = subwrd(args,3)
_dlab  = subwrd(args,4)
_print = subwrd(args,5)

if(_print = '')
   say 'SYNTAX: show-qbo_highres_row <syear> <eyear> <color> <dlab> <print>'
   say ''
   say 'syear: <start year>  - start year'
   say 'eyear: <end year>    - end year'
   say 'color: <yes|no>      - yes: create a colored shaded figure'
   say 'dlab:  <yes|no>      - yes: draw data info label'
   say 'print: <eps>         - eps: create eps file'
   return
endif
#'set display white'
lib = '/home/aktuell/local/src/grads/lib'
#file = 'qbo-plot-eraint-zm'
#
# define new colours
#
'run 'lib'/defcolbar.gs'
_colors = result
say 'colors:'_colors
if (_color = 'yes')
  cclab = '_col'
else
  cclab = ''
endif

#file = 'qbo-plot-eraint-zm'
dl = ''
if (_dlab = 'yes'); dl = '_dl'; endif
file = 'qbo-plot-sing-highres_'_syear'_'_eyear''dl

# line thickness
_lthk   = 3
_qbocol = 15

_ylevs = '100 70 50 40 30 20 15 10'
_pas = '100 70 50 30 20 10'
_pas = _ylevs
_kms = '18 22 26 30'
_kms = '17 20 23 26 29 32'

cthick = 3
'set ylopts 1 5 0.14'
'set xlopts 1 5 0.13'

_vmin = -40
_vmax = _vmin * -1
_vint = 5

sy = _syear
ey = _eyear
ny = 8
ny = 5
nrow = math_int((ey - sy + 1)/ny)
ntot = nrow * ny
say ' NROW = 'nrow' with NY 'ny' years. NROW * NY = 'ntot
nyears = _eyear - _syear + 1
say ' NUMBER OF YEARS TO DISPLAY 'nyears
if (ntot < nyears); nrow = nrow + 1; endif
#
# define the frame
#
_xbl = 0.6
_xbr = 0.6
_ybb = 1.3
_ybt = 0.1
_xs  = 0.5
_ys  = 0.6
_nximage = 1
_nyimage = 2
def_frame(_xbr' '_xbl' '_ybb' '_ybt' '_xs' '_ys' '_nximage' '_nyimage)
'set grads off'
#if(_print = 'eps')
#   'enable print 'file'.hc'
#endif

#
# data set: FUB QBO
# --------------------------------
#
dset  = 'QBO data, Freie Univ. Berlin'
dset2 = 'Institut fuer Meteorologie'
dvar = 'u'
#dvar = 'u'
lat = 0

'open ../qbo_highres.ctl'

if (_title = 'yes')
   'set string 1 tl 6'
   'set strsiz 0.18'
   'draw string '_xx1' '_yy2+0.3' QBO'
endif
nroff = _nyimage - nrow
irow = 0
while (irow < nrow)
  _nxim = 1
  _nyim = _nyimage - nroff - irow; 
  _syear = sy + irow * ny
  _eyear = _syear + ny-1
  say '_syear = '_syear' _eyear = '_eyear' _nyim = '_nyim' myrow = 'myrow' irow = 'irow
  if (irow = nrow-1)
     cbar = 'yes'
  else
     cbar = 'no'
  endif
  plot_qbo(dvar' 'lat' 'cbar)
  irow = irow + 1
endwhile
if (_dlab = 'yes')
  'set string 1 tr 5'
  'set strsiz 0.11'
  'draw string '_xx2' '_yy1-0.5' Data: 'dset
endif
if(_print = 'eps')
'set display white'
#   'print 'file'.hc'
#   'disable print 'file'.hc'
#   '!gxeps 'ceps' -i 'file'.hc -o eps/'file'.eps'
   'gxprint eps/'file'.eps white'
   '!epstopdf eps/'file'.eps'
   '!pdfcrop eps/'file'.pdf'
   '!mv eps/'file'-crop.pdf eps/'file'.pdf'
#   '!rm 'file'.hc'
   say 'eps/'file'.eps'
endif

function plot_qbo(args)

dvar = subwrd(args,1)
lat  = subwrd(args,2)
cbar = subwrd(args,3)
# SOI
# ############
def_page(_xbl' '_ybb' '_xs' '_ys' '_nxim' '_nyim)

cold = 0.5

'set lat 'lat
'set lev 100 10'
'set zlog on'
#'set time 01jan'_syear' 01jan'_eyear+1
'set time 15jan'_syear' 15dec'_eyear
'set ylevs '_ylevs
#if ( (_eyear-_syear) < 30)
# ################################################# #
# create x-label 
# (function cr_xl.gsf in /home/kunze/src/lib/grads)
# ################################################# #
   cr_xl(_syear' '_eyear)
#endif
'set gxout shaded'
if (_color != 'yes')
  'set clevs 0'
  'set rbcols 0 '_qbocol
  'd 'dvar
  'set gxout contour'
  'set ccolor 1'
  'set cthick '_lthk
  'set cint 10'
  'set clab forced'
  'd 'dvar
else
  'set rbcols '_colors
  'set rbrange '_vmin' '_vmax
  'set cint '_vint
  'd 'dvar
  _xpos = _xx1 + (_xx2 - _xx1)*0.5
  if (_dlab = 'yes')
    _ypos = _yy1-0.95
  else
    _ypos = _yy1-0.6
  endif
  if (cbar = 'yes')
    'run cbarn 1 0 '_xpos' '_ypos''
    'draw string '_xx2-0.2' '_ypos' m/s'
  endif
  'set gxout contour'
  'set ccolor 1'
  'set cthick '_lthk
  'set clevs 0'
  'set clab on'
  'd 'dvar
endif
km(_xx1' '_xx2' '_yy1' '_yy2)


#'set gxout shaded'
#'set clevs 0'
#'set rbcols 0 '_qbocol
#'d 'dvar
#'set gxout contour'
#'set ccolor 1'
#'set cthick '_lthk
#'set cint 10'
#'set clab forced'
#'d 'dvar

return

#********************************
#********************************
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

# *************************
# km Skala
# *************************
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
'set string 1 tc 5 -270' 
'set strsiz 0.13'
ypos = yy1 + (yy2 - yy1)*0.5
'draw string 'xx2+0.4' 'ypos' km'
'draw string 'xx1-0.55' 'ypos' hPa'
'set string 1 tc 5 0' 
'set ylpos 0 l'
'set zlog on'
'set dfile 1'
'close 2'
'set z 1 15'
'set ylevs '_pas
'set xlab on'
return
