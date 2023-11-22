; $Id: $

function READ_QBO_RAOBS, JD = jd, JS = js, PRESS = press, ALT = alt, THERM = therm, $
                         TEMP = temp, MERID = merid, DATAHOME = datahome,           $
                         CANTON = canton, EARLY_SINGAPORE = early_singapore,        $
                         SINGAPORE = singapore, HELP = hlp

;+
; NAME:
;       READ_QBO_RAOBS
;
; PURPOSE:
;       Read monthly mean QBO zonal wind, meridional wind or
;       temperature data from rawinsonde observations.
;
; CATEGORY:
;       Input/output.
;
; CALLING SEQUENCE:
;       DATA = READ_QBO_RAOBS()
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
      print, "Read monthly mean QBO data from radiosonde data sets."
      print, 'data = read_qbo_raobs()'
      print, '  data     = monthly mean wind (in m/s) or temperature (in C).        out'
      print, 'Keywords:'
      print, '  jd       = time coordinates in Julian days.                         out'
      print, '  js       = time coordinates in Julian seconds.                      out'
      print, '  press    = pressure coordinates (in hPa).                           out'
      print, '  alt      = geometrical altitude (in m).                             out'
      print, '  therm    = thermodynamic coordinates (p/p0)^kappa).                 out'
      print, '  /merid   = return meridional wind instead of zonal wind.'
      print, '  /temp    = return temperature instead of zonal wind.'
      return, -1
   endif

;  Prepare filename base
;  ---------------------

   if n_elements(datahome) eq 0 then  $
      datahome = getenv('DATAHOME')
   if strlen(datahome) eq 0 then  $
      datahome = '/home/marq/data/raobs/ncar'

   filebase = datahome

;  Prepare fields
;  --------------

   u = 0. & v = 0. & t = 0.
   jd = 0. & js = 0.
   press = 0. & therm = 0. & altitude = 0.
   desc = ' '

;  Prepare data set to be read
;  ---------------------------

   case 1 of
      keyword_set(canton)         : file = filebase + '/canton_monthly.sav'
      keyword_set(early_singapore): file = filebase + '/singapore48694_monthly.sav'
      keyword_set(singapore)      : file = filebase + '/singapore48698_monthly.sav'
      else                        : message, 'Station must be specified!'
   endcase

;  Read data and reorganize fields
;  -------------------------------

   restore, file

   case 1 of
      keyword_set(temp) : data = t
      keyword_set(merid): data = v
      else              : data = u
   endcase

   alt = altitude

   return, data
end
