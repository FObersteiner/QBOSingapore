; $Id$

pro WSA_QBO, WIND, SHEAR, ACCELERATION, VINT = vint, ALTITUDE = alt, $
             PRESSURE = press, THERMVAR = therm,                     $
             JD = jd, JS = js, DENNIS = dennis, NAUJ = nauj, LOW = low,  $
             LINEAR = linear, POLY = poly, PORDER = porder,          $
             SPLINE = cubic_spline, SPLINT = splint,                 $
             TENSION = tension, AKIMA = akima,                       $
             FILTER = filter, SAVGOL = savgol, HELP = hlp

;+
; NAME:
;       WSA_QBO
;
; PURPOSE:
;       This routine provides the caller with the time-height fields
;       of tropical QBO winds, vertical shears and effective acceleration.
;       Optionally, arrays containing times of individual profiles as well
;       as the vertical coordinates of the levels (in pressure, meters or
;       the thermodynamic coordinate) can be returned.
;
; CATEGORY:
;       Analysis.
;
; CALLING SEQUENCE:
;       WSA_QBO, WIND, SHEAR, ACCELERATION
;
; INPUTS:
;       None.
;
; OPTIONAL INPUTS:
;       
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;       WSACOM:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;       Mon May 27 01:21:06 1996, Christian Marquardt <marq@kassandra>
;
;		Added commonblock wsacom for faster access on already
;               calculated data.
;
;       Sun Mar 24 13:38:33 1996, Christian Marquardt <marq@kassandra>
;
;               Created.
;
;
;-

;  Give some help
;  --------------

   if n_params() eq 0 or keyword_set(hlp) then begin
      print, 'Read wind, shear and acceleration from the QBO data set.'
      print, 'wsa_qbo, wind, shear, accel, jd=, js=, press=, alt=, therm='
      print, '  wind   = zonal winds (m/s).                                out'
      print, '  shear  = vertical wind shear (m/s/km).                     out'
      print, '  accel  = net acceleration (m/s/d).                         out'
      print, 'Keywords:'
      print, '  vint   = number of vertical levels (default is 14).         in'
      print, '  jd     = Julian days of vertical profiles.                 out'
      print, '  js     = Julian seconds of vertical profiles.              out'
      print, '  press  = pressure levels (hPa).                            out'
      print, '  alt    = altitude levels (km).                             out'
      print, '  therm  = thermodynamic coordinate ((p/p0)^kappa)).         out'
      print, '  filter = array with filter weights to be applied to data.  in'
      print, '  savgol = array with parameters for Savitzky-Golay filter'
      print, '             to be applied to data.                          in'
      print, '  /nauj   use original data from Naujokat.' 
      print, '  /shea   use data from Shea.'
      print, "  /low    use Naujokat's low resolution data set (7 levels)."
      print, '  /linear linear interpolation in the vertical.'
      print, '  /poly   polynomial interpolation.'
      print, '    porder = order of interpolation polynomial to use.       in'
      print, '  /spline cubic spline interpolation.'
      print, '  /splint spline with tension.'
      print, '    tension = tension of spline to use.                      in'
      print, '  /akima  Akima spline interpolation.'
      print, 'Note: Filters are applied to the data at each level, and'
      print, '  any interpolation is done afterwards.'
      return
   endif
      
;
;  Common block containing already read data
;  -----------------------------------------
;
   common wsacom, p_wind, p_shear, p_accel, p_vint, p_filter, p_savgol,  $
                  p_alt, p_press, p_therm

   if n_elements(p_wind)   eq 0  then p_wind   = 0.
   if n_elements(p_shear)  eq 0  then p_shear  = 0.
   if n_elements(p_accel)  eq 0  then p_accel  = 0.
   if n_elements(p_vint)   eq 0  then p_vint   = 0
   if n_elements(p_filter) eq 0  then p_filter = 0
   if n_elements(p_savgol) eq 0  then p_savgol = 0


;  Read QBO data (joined data is default)
;  --------------------------------------

   if keyword_set(nauj) then begin
      if keyword_set(low) then begin
         wind = read_qbo_nauj(alt = alt, therm = therm, press = press,  $
                              jd = jd, js = js, /low, /nan)
      endif else begin
         wind = read_qbo_nauj(alt = alt, therm = therm, press = press,  $
                              jd = jd, js = js, /nan)
      endelse
   endif else if keyword_set(dennis) then begin
      wind = read_qbo_shea(alt = alt, therm = therm, press = press,  $
                           jd = jd, js = js, /nan)
   endif else begin
      wind = read_qbo(alt = alt, therm = therm, press = press,  $
                      jd = jd, js = js, /nan)
   endelse

   n_times = n_elements(jd)

;  Filter
;  ------

   filtxt = ''
   if n_elements(filter) ne 0 then begin
      if filter ne p_filter then begin
         print, 'Filtering...'
         wind = filter_qbo(wind, filter = filter, filtxt = filtxt)
         p_filter = filter
      endif
   endif else if n_elements(savgol) ne 0 then begin
      if savgol ne p_savgol then begin
         print, 'Filtering...'
         wind = filter_qbo(wind, savgol = savgol, filtxt = filtxt)
         p_savgol = savgol
      endif
   endif


;  Interpolations...
;  -----------------

   inttxt = ''
   if n_elements(vint) eq 1 then begin
      if vint ne p_vint then begin
         print, "Interpolating..."
         if not keyword_set(linear)       then linear = 0
         if not keyword_set(poly)         then poly = 0
         if n_elements(porder) ne 1       then porder = 3
         if not keyword_set(cubic_spline) then cubic_spline = 0
         if not keyword_set(splint)       then splint = 0
         if n_elements(tension) ne 1      then tension = 1.0
         if not keyword_set(akima)        then akima = 0
         altitudes = maken(alt(0), alt(n_elements(alt)-1), vint)
         pressures = 1000. * exp(- altitudes/7000.)
         therm = (pressures/1000.)^(2./7.)
         iwind = interp_qbo(wind, jd, alt, altitudes, inttxt = inttxt,     $
                            linear = linear, poly = poly, porder = porder, $
                            spline = cubic_spline, splint = splint,        $
                            tension = tension, akima = akima)
         wind  = iwind                     & p_wind  = wind
         alt   = altitudes                 & p_alt   = alt
         press = 1000. * exp(- alt/7000.)  & p_press = press
         therm = (press/1000.)^(2./7.)     & p_therm = therm
         p_shear = 0.
         p_accel = 0.
         p_vint  = vint
      endif else begin
         wind  = p_wind
         alt   = p_alt
         press = p_press
         therm = p_therm
      endelse
   endif else begin
      if p_vint ne 0 then begin
         p_shear = 0
         p_accel = 0
         p_vint  = 0
      endif
      p_wind  = wind
      p_alt   = alt
      p_press = press
      p_therm = therm
   endelse


;  Shear
;  -----

   if n_elements(p_shear) ne n_elements(p_wind) then begin
      print, "Calculating shear..."
      shear = wind
      for index = 0, n_times-1 do begin
         idx = where(finite(shear(index, *)) eq 1, count)
         if count gt 2 then $
            shear(index, *) = dydx2(alt, wind(index, *))
      endfor
      shear = shear * 1000. &  p_shear = shear
   endif else begin
      shear = p_shear
   endelse


;  Acceleration
;  ------------

   if n_elements(p_accel) ne n_elements(p_wind) then begin
      print, "Calculating acceleration..."
      n_levels = n_elements(alt)
      acceleration = wind
      for index = 0, n_levels-1 do begin
         acceleration(*, index) = dydx2(jd, wind(*, index))
      endfor
      p_accel = acceleration
   endif else begin
      acceleration = p_accel
   endelse

end

