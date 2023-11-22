; $Id: $

function READ_QBO_NCEP, JD = jd, JS = js, PRESS = press, ALT = alt, THERM = therm, $
                        TEMP = tmp, SHEAR = shear, ACCEL = accel,                  $
                        ORIG = orig, DETREND = detrd, DESEAS = deseas,             $
                        DATAHOME = datahome, HELP = hlp

;+
; NAME:
;       READ_QBO_NCEP
;
; PURPOSE:
;       Read monthly mean Equatorial zonal wind or temperature data
;       from the NCEP/NCAR Reanalysis project. 
;
; CATEGORY:
;       Input/output.
;
; CALLING SEQUENCE:
;       DATA = READ_QBO_NCEP()
; 
; INPUTS:
;       None.
;
; OPTIONAL INPUTS:
;       None.
;	
; KEYWORD PARAMETERS:
;       /TEMP:  Return temperature instead of zonal wind.
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
      print, "Read NCEP/NCAR Reanalysis monthly mean equatorial data."
      print, 'data = read_qbo_ncep()'
      print, '  data     = monthly mean zonal wind (in m/s) or temperature (in C).  out'
      print, 'Keywords:'
      print, '  jd       = time coordinates in Julian days.                         out'
      print, '  js       = time coordinates in Julian seconds.                      out'
      print, '  press    = pressure coordinates (in hPa).                           out'
      print, '  altitude = geometrical altitude (in m).                             out'
      print, '  therm    = thermodynamic coordinates (p/p0)^kappa).                 out'
      print, '  /temp    = return temperature instead of zonal wind.'
   endif

;  Prepare filename base
;  ---------------------

   if n_elements(datahome) eq 0 then  $
      datahome = getenv('DATAHOME')
   if strlen(datahome) eq 0 then  $
      datahome = '/home/marq/data/ncep'

   filebase = datahome

;  Prepare coordinate vectors and fields
;  -------------------------------------

   press = 0. & lat = 0. & lon = 0. & jd = 0l & js = 0.d0
   ugrd = 0. & vgrd = 0. & temp = 0. & height = 0. & desc = ''

   case 1 of
      keyword_set(tmp) : file = filebase + '/ncep_zm.temp.sav'
      keyword_set(hgt) : file = filebase + '/ncep_zm.height.sav'
      keyword_set(vwn) : file = filebase + '/ncep_zm.vgrd.sav'
      else             : file = filebase + '/ncep_zm.ugrd.sav'
   endcase

   restore, file

   alt   = - 7000. * alog(press/1000.)
   therm = (press/1000.)^(2./7.)

   eq_idx = where(lat eq 0.)

   case 1 of
      keyword_set(tmp) : data = temp(eq_idx, *, *)
      keyword_set(hgt) : data = height(eq_idx, *, *)
      keyword_set(vwn) : data = vgrd(eq_idx, *, *)
      else             : data = ugrd(eq_idx, *, *)
   endcase
   data = transpose(reform(data))

;  Care for interpolated values
;  ----------------------------

   if not keyword_set(orig) then begin
      press_i = [100., 90., 80., 70., 60., 50., 45., 40.,  $
                  35., 30., 25., 20., 15., 12., 10.]
      alt_i   = -7000. * alog(press_i/1000.)
      data    = interp_qbo(data, jd, alt, alt_i, /spline)
      press   = press_i
      alt     = alt_i
      therm   = (press/1000.)^(2./7.)
   endif

;  Shear
;  -----

   if keyword_set(shear) then begin
      for index = 0, n_elements(jd) - 1 do begin
         idx = where(finite(data(index, *)) eq 1, count)
         if count gt 2 then $
            data(index, *) = dydx2(alt, data(index, *))
      endfor
      data = data * 1000.
   endif

;  Acceleration
;  ------------

   if keyword_set(accel) then begin
      for index = 0, n_elements(alt) - 1 do begin
         data(*, index) = dydx2(jd, data(*, index))
      endfor
   endif

;  Care for detrending
;  -------------------

   if keyword_set(detrd) then begin
      data = detrend(data, jd, tdim = 1)
   endif

;  Care for deseasoning
;  --------------------

   if keyword_set(deseas) then begin
      data = deseason(data, jd, tdim = 1)
   endif

   return, data
end
