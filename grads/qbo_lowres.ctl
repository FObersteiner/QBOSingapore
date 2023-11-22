dset ^qbo_lowres.gra
options little_endian
title Monthly mean zonal wind components u (m/s) 
undef -99999.
xdef 1 linear 0 1
ydef 1 linear 0 1
zdef  8 levels    100    70    50    40    30    20    15    10
tdef 840 linear 01jan1953 1mo
vars 1
u  8 99 wind component u (m/s)
endvars
