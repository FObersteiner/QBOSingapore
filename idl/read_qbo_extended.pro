; $Id: $

function READ_QBO_EXTENDED, JD = jd, JS = js, PRESS = press, ALT = alt, THERM = therm, $
                            TEMP = temp, MERID = merid, SHEAR = shr, ACCEL = acc,  $
                            HELP = hlp

;+
; NAME:
;       READ_QBO_EXTENDED
;
; PURPOSE:
;       Read monthly mean QBO zonal wind, meridional wind or
;       temperature data from the extended data set.
;
; CATEGORY:
;       Input/output.
;
; CALLING SEQUENCE:
;       DATA = READ_QBO_EXTENDED()
; 
; INPUTS:
;       None.
;
; OPTIONAL INPUTS:
;       None.
;	
; KEYWORD PARAMETERS:
;       /TEMP:  Return temperature instead of zonal wind.
;       /MERID: Return meridional wind instaed of zonal wind.
;
; OUTPUTS:
;       DATA:   Wind or temperature data
;
; OPTIONAL OUTPUTS:
;       JD:
;       JS:
;       PRESS:
;       ALT:
;       THERM:
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;-

;  Give some help
;  --------------

   if keyword_set(hlp) then begin
      print, "Read monthly mean QBO data from the extended radiosonde data sets."
      print, 'data = read_qbo_extended()'
      print, '  data     = monthly mean wind (in m/s) or temperature (in C) or...   out'
      print, 'Keywords:'
      print, '  jd       = time coordinates in Julian days.                         out'
      print, '  js       = time coordinates in Julian seconds.                      out'
      print, '  press    = pressure coordinates (in hPa).                           out'
      print, '  alt      = geometrical altitude (in m).                             out'
      print, '  therm    = thermodynamic coordinates (p/p0)^kappa).                 out'
      print, '  /merid   = return meridional wind instead of zonal wind.'
      print, '  /temp    = return temperature instead of zonal wind.'
      print, '  /shear   = return vertical wind shear instead of zonal wind.'
      print, '  /accel   = return net accelaration instead of zonal wind.'
      return, -1
   endif

;  

;  Prepare filename base
;  ---------------------

   if n_elements(datahome) eq 0 then  $
      datahome = getenv('DATAHOME')
   if strlen(datahome) eq 0 then  $
      datahome = '/home/aktuell/idl/qbo'

   filebase = datahome

;  Prepare fields
;  --------------

   u = 0. & v = 0. & t = 0. & shear = 0. & accel = 0.
   jd = 0. & js = 0.
   press = 0. & therm = 0. & altitude = 0.
   desc = ' '

;  Prepare data set to be read
;  ---------------------------

   file = filebase + '/qbo_extended.sav'

;  Read data and reorganize fields
;  -------------------------------

   restore, file

   case 1 of
      keyword_set(temp) : data = t
      keyword_set(merid): data = v
      keyword_set(shr)  : data = shear
      keyword_set(acc)  : data = accel
      else              : data = u
   endcase

   alt = altitude

   return, data
end
