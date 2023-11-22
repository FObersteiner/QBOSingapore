 &RESOLUTION  RES       = "DEF",
              LONS_IN   =  0.,
              LATS_IN   =  0.,
              TRES      = "mo",
              DELTA     =   1,
              ZONALMEAN = T,
              YREV      = F
 /
 &LEVELS      LEVEL    = 100,    70,    50,    40,    30,    20,    15,    10,
              UNIT_IN  = "HPA",
              UNIT_OUT = "HPA"
 /
 &CODE        MISSING  = -99999.
              CODES    = "xu",
              UNITS    = "m/s"
              NCCODES  = "u" 
 /
 &TIMES       NTIME     = 840,
              FIRST_DAT = "19530115",
              TSTEP     = 360
 /
 &WORK        NMEAN     =   1,
              OFFSET_B  =   0,
              OFFSET_E  =   0,
              LZM_OUT   = F,
              LSTDDEV   = F,
              LCUT      = F,
              SLON      =  -9.9990E+30,
              ELON      =  -9.9990E+30,
              SLAT      =  -9.9990E+30,
              ELAT      =  -9.9990E+30,
              LVARMEAN  = F,
              CODE_MEAN = "",
              VARFACT   = 1, 1, 1
 /
 &FILES       NINFILE  =   1,
              NOUTFILE =   1,
              INFILE   = "../grads/qbo_lowres.gra",
              OUTFILE  = "qbo_lowres.nc"
 /
