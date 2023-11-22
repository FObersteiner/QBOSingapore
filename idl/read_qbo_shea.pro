; $Id: $

function READ_QBO_SHEA, JD = jd, JS = js, PRESS = press, ALT = alt, THERM = therm, $
                        TEMP = temp, ORIG = orig, DETREND = detrd, DESEAS = deseas, $
                        TRENSO_FIT = trenso_fit, STRENSO_FIT = strenso_fit,  $
                        NAN = nan, FIXGAP = fixgap, $
                        DATAHOME = datahome, HELP = hlp

;+
; NAME:
;       READ_QBO_SHEA
;
; PURPOSE:
;       Read Sheas monthly mean QBO zonal wind or temperature data
;       set. 
;
; CATEGORY:
;       Input/output.
;
; CALLING SEQUENCE:
;       DATA = READ_QBO_SHEA()
; 
; INPUTS:
;       None.
;
; OPTIONAL INPUTS:
;       None.
;	
; KEYWORD PARAMETERS:
;       /TEMP:  Return temperature instead of zonal wind.
;       /NAN:   Set missing values to NaN.
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
      print, "Read Dennis Sheas monthly mean QBO data."
      print, 'data = read_qbo_shea()'
      print, '  data     = monthly mean zonal wind (in m/s) or temperature (in C).  out'
      print, 'Keywords:'
      print, '  jd       = time coordinates in Julian days.                         out'
      print, '  js       = time coordinates in Julian seconds.                      out'
      print, '  press    = pressure coordinates (in hPa).                           out'
      print, '  altitude = geometrical altitude (in m).                             out'
      print, '  therm    = thermodynamic coordinates (p/p0)^kappa).                 out'
      print, '  /temp    = return temperature instead of zonal wind.'
      print, '  /orig    = return original levels of the data set, not the interpolated'
      print, '               stratospheric ones.'
      print, "  /nan     = missing values are returned as NaNs (default is -99.9)."
      return, -1
   endif

;  Prepare filename base
;  ---------------------

   if n_elements(datahome) eq 0 then  $
      datahome = getenv('DATAHOME')
   if strlen(datahome) eq 0 then  $
      datahome = '/home/aktuell/data/qbo/data'

   filebase = datahome

;  Prepare coordinate vectors and fields
;  -------------------------------------

   press = float([1000, 850, 700, 500, 400, 300, 250, 200, 150, 100, 70, 50, 30, 20, 10])
   alt   = - 7000. * alog(press/1000.)
   therm = (press/1000.)^(2./7.)

   jd = lonarr((1996-1957 + 1) * 12) & idx = 0

   for year = 1957, 1996 do begin
      for month = 1, 12 do begin
         jd(idx) = ymd2jd(year, month, 15.)
         idx = idx + 1
      endfor
   endfor

   js = jd2js(jd)

   data = fltarr(n_elements(jd), n_elements(press))

   tmp_data = fltarr(n_elements(press))

   station = 0 & type = 0 & yr = 0 & mo = 0

;  Read the data
;  -------------

   if keyword_set(temp) then begin
      openr, lun, filebase + '/shea/qbo_t', /get_lun
   endif else begin
      openr, lun, filebase + '/shea/qbo_u', /get_lun
   endelse

   for idx = 0, n_elements(jd) - 1 do begin
      readf, lun, format = '(i7,i2,2i3,15i5)', station, type, yr, mo, tmp_data
      data(idx, *) = tmp_data / 10.
   endfor

   free_lun, lun

;  Care for missing values
;  -----------------------

   idx = where(data eq -99.9, count)
   if count ne 0 then data(idx) = !values.f_nan

;  Care for fixing gaps
;  --------------------

   if keyword_set(fixgap) then begin
      for lvl = 0, n_elements(press) - 1 do begin
         tmp = data(*, lvl)
         idx = where(finite(tmp) eq 1)
         tmp = interpx(jd(idx), tmp(idx), jd, bad = !values.f_nan, gap = 6*31)
         data(*, lvl) = tmp
      endfor
   endif
      
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

;  Individual stuff for the stations
;  ---------------------------------

   if not keyword_set(together) then begin

;      print, 'Seperately for Canton Island and Singapore:'

      canton_range = where(jd le ymd2jd(1967,  9, 15))
      singap_range = where(jd ge ymd2jd(1967, 10, 15))

;      Care for deseasoning 
;      --------------------

      if keyword_set(deseas) then begin
         print, '   deseasonalizing...'
         data(canton_range, *) = deseason(data(canton_range, *), jd(canton_range), tdim = 1)
         data(singap_range, *) = deseason(data(singap_range, *), jd(singap_range), tdim = 1)
      endif

;     Care for detrending 
;     ------------------- 

      if keyword_set(detrd) then begin 
         print, '   detrending...'
         data(canton_range, *) = detrend(data(canton_range, *), jd(canton_range), tdim = 1)
         data(singap_range, *) = detrend(data(singap_range, *), jd(singap_range), tdim = 1)
      endif

;     Care for TRENSO fits
;     --------------------

      if keyword_set(strenso_fit) then begin
         print, '   Simplified TRENSO fitting...'

         fit_strenso, data(canton_range, *), jd(canton_range), fitted = fitted, $
            jd_zero = ymd2jd(1967, 10, 15), /filt
         data(canton_range, *) = data(canton_range, *) - fitted

         fit_strenso, data(singap_range, *), jd(singap_range), fitted = fitted, $
            jd_zero = ymd2jd(1967, 10, 15), /filt
         data(singap_range, *) = data(singap_range, *) - fitted
      endif

      if keyword_set(trenso_fit) then begin
         print, '   TRENSO fitting...'

         fit_trenso, data(canton_range, *), jd(canton_range), fitted = fitted, $
            jd_zero = ymd2jd(1967, 10, 15), /filt
         data(canton_range, *) = data(canton_range, *) - fitted

         fit_trenso, data(singap_range, *), jd(singap_range), fitted = fitted, $
            jd_zero = ymd2jd(1967, 10, 15), /filt
         data(singap_range, *) = data(singap_range, *) - fitted
      endif

   endif else begin
      
;     Care for detrending
;     -------------------

      if keyword_set(detrd) then begin
         data = detrend(data, jd, tdim = 1)
      endif

;     Care for deseasoning
;     --------------------

      if keyword_set(deseas) then begin
         data = deseason(data, jd, tdim = 1)
      endif

   endelse

   return, data
end
