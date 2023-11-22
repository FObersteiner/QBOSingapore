; $Id$

pro QBO, VINT, SHEAR = shear_s, ACCEL = accel_s, TEMP = temp,   $
         LOW = low, LANDSCAPE = landscape, COMPACT = compact,   $
         STARTDATE = startdate, ENDDATE = enddate,              $
         REDUCE = reduce, EXTENDED = extended, NCEP = ncep,     $
         THERMAL_FIT = thermal_fit, THERMAL_RES = thermal_res,  $
         DIAG = diag, TRANS = trans, FACTOR = factor,           $
         LFILL = lfill, COLOR = color, SFIT = sfit,             $
         FFILTER = ffilter, LPASS = lpass, HPASS = hpass,       $
         COFILTER = cofilter, HBAR = hbar, VBAR = vbar,         $
         _EXTRA = extra

;+
; NAME:
;       QBO
;
; PURPOSE:
;       This routine plots a full page plot of the QBO time series or
;       one of its derivatives.
;
; CATEGORY:
;       Plotting.
;
; CALLING SEQUENCE:
;       QBO [, VINT] [, /LOW] [, /LANDSCAPE], [/LFILL] [,/COLOR] $
;           [, <interpolation option>] [, <filter option>]
;
; INPUTS:
;       None.
;
; OPTIONAL INPUTS:
;       VINT:
;
; KEYWORD PARAMETERS:
;       START: String containing a (valid) startdate.
;       END:   String containing a (valid) enddate) if both START and
;                END are given, only one plot is drawn in the present
;                graphic window.
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       Draws a plot.
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;       Wed Mar 6 12:06:21 1996, Christian Marquardt <marq@kassandra>
;
;		Added support for interpolation options.
;
;       During Feb 1996,         Christian Marquardt <marq@kassandra>
;
;               Created.
;
;-

;  Default arguments
;  -----------------

   if n_elements(hbar) ne 2 then hbar = 0.

;  Read QBO data and do all interpolations / filterings
;  ----------------------------------------------------

   if keyword_set(extended) then begin
      wind  = read_qbo_extended(jd = jd, js = js, $
                                alt = alt, press = press, therm = therm)
      shear = read_qbo_extended(jd = jd, js = js, $
                                alt = alt, press = press, therm = therm, /shear)
      accel = read_qbo_extended(jd = jd, js = js, $
                                alt = alt, press = press, therm = therm, /accel)
      temperature = read_qbo_extended(jd = jd, js = js, $
                                      alt = alt, press = press, therm = therm, /temp)
   endif else if keyword_set(ncep) then begin
      wind  = read_qbo_ncep(jd = jd, js = js, alt = alt, press = press, therm = therm)
      shear = read_qbo_ncep(/shear)
      accel = read_qbo_ncep(/accel)
      temperature  = read_qbo_ncep(jd = jd_temp, js = js_temp, /temp, /deseas)
   endif else if keyword_set(diag) then begin
      print, "Using diagnostics..."
      diag_qbo, alpha = 1. / (25.*24.*60.*60.), wind = wind, shear = shear,  $
         wave_driving = accel, t_qbo = temperature, jd = jd, js = js, $
         alt = alt, therm = therm, press = press
   endif else begin
      if n_elements(vint) ne 1 then begin
         wsa_qbo, wind, shear, accel, jd = jd, therm = therm, $
            press = press, _extra = extra
         temperature = read_qbo_shea(jd = jd_temp, therm = therm, /temp, $
                                     /fixgap, /deseas, /nan)
      endif else begin
         wsa_qbo, wind, shear, accel, vint = vint, jd = jd, therm = therm, $
            press = press, _extra = extra
         temperature = read_qbo_shea(jd = jd_temp, therm = therm, /temp,  $
                                     /fixgap, /deseas, /nan)
      endelse
   endelse

;  A special case: Fourier filtering
;  ---------------------------------

   if keyword_set(ffilter) then begin
      if keyword_set(lpass) then begin
         print, 'Fourier filtering (lowpass)...'
         weight_w = cosfilt(wind(*, 1), 1./1000., 1./100., 1./6.0001, 1./6.)
         weight_t = cosfilt(temperature(*, 1), 1./1000., 1./100., 1./12.0001, 1./12.)
      endif else if keyword_set(hpass) then begin
         print, 'Fourier filtering (highpass)...'
         weight_w = cosfilt(wind(*, 1), 1./40.0001, 1./40., 1./2.0001, 1./2.00001)
         weight_t = cosfilt(temperature(*, 1), 1./40.0001, 1./40., 1./2.0001, 1./2.00001)
      endif else begin
         print, 'Fourier filtering (bandpass)...'
         weight_w = cosfilt(wind(*, 1), 1./40.0001, 1./40., 1./12.0001, 1./12.)
         weight_t = cosfilt(temperature(*, 1), 1./40.0001, 1./40., 1./12.0001, 1./12.)
      endelse
      if keyword_set(cofilter) then begin
         print, 'Using 1. - filter...'
         weight_w =  1. - weight_w
         weight_t =  1. - weight_t
      endif
      for l = 0, n_elements(press)-1 do begin
         wind(*, l)         = float(fft_filter(wind(*, l), weight_w))
         shear(*, l)        = float(fft_filter(shear(*, l), weight_w))
         temperature(*, l)  = float(fft_filter(temperature(*, l), weight_t))
      endfor
   endif

;  Transition regions
;  ------------------

   if keyword_set(trans) then begin
      if n_elements(factor) ne 1 then factor = 0.75
      transit = fltarr(n_elements(jd), n_elements(press))
      tmp     = fltarr(n_elements(jd))
      for lvl = 0, n_elements(press) - 1 do begin
         idx  = transitions(press(lvl), wind(*, lvl), factor)
         if idx(0) ne -1 then begin
            tmp(idx) = replicate(1., n_elements(idx))
            transit(*, lvl) = tmp
            tmp = 0.*tmp
         endif
      endfor
   endif

;  Several temperature fits
;  ------------------------
   if keyword_set(temp) and keyword_set(sfit) then begin
      fit_trenso, temperature, jd_temp, fitted = fit
      temperature = temperature - fit
   endif

   if keyword_set(temp) and keyword_set(thermal_fit) then begin
      thermal, /all, /trenso, /deseas, /noplot, jd = jd, fittemp = temperature,  $
         ncep = ncep
   endif

   if keyword_set(temp) and keyword_set(thermal_res) then begin
      thermal, /all, /trenso, /deseas, /noplot, jd = jd_temp, $
         temp = temperature, fittemp = fit, ncep = ncep
      if keyword_set(sfit) then begin
         fit_trenso, temperature, jd_temp, fitted = fit2
         temperature = temperature - fit2
      endif
      temperature = temperature - fit
   endif

   if keyword_set(reduce) then begin
      wind  =  wind  - ( replicate(1., n_elements(jd)) # mean(wind, 1))
      shear =  shear - ( replicate(1., n_elements(jd)) # mean(shear, 1))
   endif

;  Reset missing values (i.e, NaN's) to high values
;  ------------------------------------------------

   idx = where(finite(wind) eq 0, count) & if count gt 0 then wind(idx) = 99.9
   idx = where(finite(accel) eq 0, count) & if count gt 0 then accel(idx) = 99.9
   idx = where(finite(shear) eq 0, count) & if count gt 0 then shear(idx) = 99.9

;  Check for given start and end dates
;  -----------------------------------

   if n_elements(startdate) eq 1 and n_elements(enddate) eq 1 then begin
      only_one = 1
   endif
   
;  Prepare y-axis
;  --------------

   pressrange = [100., 10.]
   thermrange = (pressrange/1000.)^(2./7.)

   lpress = [100., 70., 50., 40., 30., 20., 15., 10.]
   ltherm = (lpress/1000.)^(2./7.)
   spress = string(lpress, format = '(i3)')
   spress(3) = ' ' &  spress(6) = ' '

   lalt = [18., 22., 26., 30.]
   salt = string(lalt, format = '(i3)')
   ltherma = exp(-lalt/7.)^(2./7.)

;
;  Prepare t-axis
;  --------------
;
   if keyword_set(landscape) then begin
      t_range = lonarr(5, 2)
      t_range(0, *) = [ymd2jd(1953, 1, 1), ymd2jd(1961, 12, 31)]
      t_range(1, *) = [ymd2jd(1962, 1, 1), ymd2jd(1970, 12, 31)]
      t_range(2, *) = [ymd2jd(1971, 1, 1), ymd2jd(1979, 12, 31)]
      t_range(3, *) = [ymd2jd(1980, 1, 1), ymd2jd(1988, 12, 31)]
      t_range(4, *) = [ymd2jd(1989, 1, 1), ymd2jd(1997, 12, 31)]
   endif else if keyword_set(compact) then begin
      t_range = lonarr(7, 2)
      t_range(0, *) = [ymd2jd(1952, 1, 1), ymd2jd(1958, 12, 31)]
      t_range(1, *) = [ymd2jd(1959, 1, 1), ymd2jd(1965, 12, 31)]
      t_range(2, *) = [ymd2jd(1966, 1, 1), ymd2jd(1972, 12, 31)]
      t_range(3, *) = [ymd2jd(1973, 1, 1), ymd2jd(1979, 12, 31)]
      t_range(4, *) = [ymd2jd(1980, 1, 1), ymd2jd(1986, 12, 31)]
      t_range(5, *) = [ymd2jd(1987, 1, 1), ymd2jd(1993, 12, 31)]
      t_range(6, *) = [ymd2jd(1994, 1, 1), ymd2jd(2000, 12, 31)]
   endif else if keyword_set(only_one) then begin
      t_range = lonarr(1, 2)
      date2ymd, startdate, syear, smonth, sday
      date2ymd, enddate,   eyear, emonth, eday
      t_range(0, *) = [ymd2jd(syear, smonth, sday), ymd2jd(eyear, emonth, eday)]
   endif else begin
;      t_range = lonarr(8, 2)
;      t_range(0, *) = [ymd2jd(1953, 1, 1), ymd2jd(1958, 12, 31)]
;      t_range(1, *) = [ymd2jd(1959, 1, 1), ymd2jd(1964, 12, 31)]
;      t_range(2, *) = [ymd2jd(1965, 1, 1), ymd2jd(1970, 12, 31)]
;      t_range(3, *) = [ymd2jd(1971, 1, 1), ymd2jd(1976, 12, 31)]
;      t_range(4, *) = [ymd2jd(1977, 1, 1), ymd2jd(1982, 12, 31)]
;      t_range(5, *) = [ymd2jd(1983, 1, 1), ymd2jd(1988, 12, 31)]
;      t_range(6, *) = [ymd2jd(1989, 1, 1), ymd2jd(1994, 12, 31)]
;      t_range(7, *) = [ymd2jd(1995, 1, 1), ymd2jd(2000, 12, 31)]
;
;      t_range = lonarr(8, 2)
;      t_range(0, *) = [ymd2jd(1953, 1, 1), ymd2jd(1959, 12, 31)]
;      t_range(1, *) = [ymd2jd(1960, 1, 1), ymd2jd(1966, 12, 31)]
;      t_range(2, *) = [ymd2jd(1967, 1, 1), ymd2jd(1973, 12, 31)]
;      t_range(3, *) = [ymd2jd(1974, 1, 1), ymd2jd(1980, 12, 31)]
;      t_range(4, *) = [ymd2jd(1981, 1, 1), ymd2jd(1987, 12, 31)]
;      t_range(5, *) = [ymd2jd(1988, 1, 1), ymd2jd(1994, 12, 31)]
;      t_range(6, *) = [ymd2jd(1995, 1, 1), ymd2jd(2001, 12, 31)]
;      t_range(7, *) = [ymd2jd(2002, 1, 1), ymd2jd(2008, 12, 31)]
;
;      t_range = lonarr(8, 2)
;      t_range(0, *) = [ymd2jd(1953, 1, 1), ymd2jd(1960, 12, 31)]
;      t_range(1, *) = [ymd2jd(1961, 1, 1), ymd2jd(1968, 12, 31)]
;      t_range(2, *) = [ymd2jd(1969, 1, 1), ymd2jd(1976, 12, 31)]
;      t_range(3, *) = [ymd2jd(1977, 1, 1), ymd2jd(1984, 12, 31)]
;      t_range(4, *) = [ymd2jd(1985, 1, 1), ymd2jd(1992, 12, 31)]
;      t_range(5, *) = [ymd2jd(1993, 1, 1), ymd2jd(2000, 12, 31)]
;      t_range(6, *) = [ymd2jd(2001, 1, 1), ymd2jd(2008, 12, 31)]
;      t_range(7, *) = [ymd2jd(2009, 1, 1), ymd2jd(2016, 12, 31)]

      t_range = lonarr(8, 2)
      t_range(0, *) = [ymd2jd(1953, 1, 1), ymd2jd(1961, 12, 31)]
      t_range(1, *) = [ymd2jd(1962, 1, 1), ymd2jd(1970, 12, 31)]
      t_range(2, *) = [ymd2jd(1971, 1, 1), ymd2jd(1979, 12, 31)]
      t_range(3, *) = [ymd2jd(1980, 1, 1), ymd2jd(1988, 12, 31)]
      t_range(4, *) = [ymd2jd(1989, 1, 1), ymd2jd(1997, 12, 31)]
      t_range(5, *) = [ymd2jd(1998, 1, 1), ymd2jd(2006, 12, 31)]
      t_range(6, *) = [ymd2jd(2007, 1, 1), ymd2jd(2015, 12, 31)]
      t_range(7, *) = [ymd2jd(2016, 1, 1), ymd2jd(2024, 12, 31)]
   endelse


;  Prepare arguments for contour_qbo
;  ---------------------------------

   if keyword_set(lfill) then begin
      options = { LFILL : 1 }
   endif else begin
      if keyword_set(color) then begin
         color, 'red', 10
         color, 'blue', 11
         options = { COLOR1 : 10, COLOR2 : 11 }
      endif else begin
         loadct, 0
         options = { COLOR1 : 175, COLOR2 : 225 }
      endelse
   endelse

   if n_elements(extra) gt 0 then begin
      options = create_struct(options, extra)
   endif

;  Set up !p.multi
;  ---------------

   size_t_range = size(t_range)
   n_frames = size_t_range(1)

   !p.multi = [0, 0, n_frames, 0, 0]


;  Do the plots
;  ------------

   print, 'Plotting...'

   if keyword_set(trans) then begin

      tlevels = makex(-5., 5., 10.)
      ultxt = 'Transition regions'

      for frame = 0, n_frames - 1 do begin
         if frame eq 0 then begin
            contour_qbo, transit, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 0.5,                               $
                         y2tickv = ltherma, y2tickname = salt,       $
                         levels = tlevels, _extra = options,         $
                         ultxt = ultxt
         endif else if frame eq n_frames-1 then begin
            contour_qbo, transit, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 0.5,                               $
                         y2tickv = ltherma, y2tickname = salt,       $
                         levels = tlevels, _extra = options,         $
                         bltxt = filtxt, brtxt = inttxt
         endif else begin
            contour_qbo, transit, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 0.9,                               $
                         levels = tlevels, _extra = options,         $
                         y2tickv = ltherma, y2tickname = salt
         endelse
      endfor

   endif else if keyword_set(shear_s) then begin

      slevels = makex(-20., 20., 5.)
      ultxt = '!Su!R!A-!N!I z!N  in m/s / km'

      color, 'red', 5

      for frame = 0, n_frames - 1 do begin
         if frame eq 0 then begin
            contour_qbo, shear, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 5.0, shade2 = -5.0,                $
                         y2tickv = ltherma, y2tickname = salt,       $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         levels = slevels, ultxt = ultxt
            contour, /over, wind, jd, therm, level = 0.,  $
               c_linestyle = 2, c_thick = 2.
         endif else if frame eq n_frames-1 then begin
            contour_qbo, shear, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 5.0, shade2 = -5.0,                $
                         y2tickv = ltherma, y2tickname = salt,       $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         levels = slevels,                           $
                         bltxt = filtxt, brtxt = inttxt
            contour, /over, wind, jd, therm, level = 0.,  $
               c_linestyle = 2, c_thick = 2.
         endif else begin
            contour_qbo, shear, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 5.0, shade2 = -5.0,                $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         y2tickv = ltherma, y2tickname = salt,       $
                         levels = slevels
            contour, /over, wind, jd, therm, level = 0.,  $
               c_linestyle = 2, c_thick = 2.
         endelse
      endfor

   endif else if keyword_set(accel_s) then begin

      if keyword_set(diag) then begin
         slevels = makex(-1., 1., 0.1)
         ultxt = 'Wave forcing  in m/s / d'
         shade1 = 0.2
         shade2 = -0.2
      endif else begin
         slevels = makex(-1., 1., 0.1)
         ultxt = '!Su!R!A-!N!I t!N  in m/s / d'
         shade1 = 0.1
         shade2 = -0.1
      endelse

      for frame = 0, n_frames - 1 do begin
         if frame eq 0 then begin
            contour_qbo, accel, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = shade1, shade2 = shade2,           $
                         y2tickv = ltherma, y2tickname = salt,       $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         levels = slevels, ultxt = ultxt
         endif else if frame eq n_frames-1 then begin
            contour_qbo, accel, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = shade1, shade2 = shade2,           $
                         y2tickv = ltherma, y2tickname = salt,       $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         levels = slevels,                           $
                         bltxt = filtxt, brtxt = inttxt
         endif else begin
            contour_qbo, accel, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = shade1, shade2 = shade2,           $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         y2tickv = ltherma, y2tickname = salt,       $
                         levels = slevels
         endelse
      endfor

   endif else if keyword_set(temp) then begin

      slevels = makex(-10., 10., 2.)
      ultxt = 'T in K'

      for frame = 0, n_frames - 1 do begin
         if frame eq 0 then begin
            contour_qbo, temperature, jd_temp, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 2.0, shade2 = -2.0,                $
                         y2tickv = ltherma, y2tickname = salt,       $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         levels = slevels, ultxt = ultxt
         endif else if frame eq n_frames-1 then begin
            contour_qbo, temperature, jd_temp, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 2.0, shade2 = -2.0,                $
                         y2tickv = ltherma, y2tickname = salt,       $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         levels = slevels,                           $
                         bltxt = filtxt, brtxt = inttxt
         endif else begin
            contour_qbo, temperature, jd_temp, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 2.0, shade2 = -2.0,                $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         y2tickv = ltherma, y2tickname = salt,       $
                         levels = slevels
         endelse
      endfor

   endif else begin

      ;ultxt = '!Su!R!A-!N  in m/s'

      for frame = 0, n_frames - 1 do begin
         if frame eq 0 then begin
            contour_qbo, wind, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 0.,                                $
                         y2tickv = ltherma, y2tickname = salt,       $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         ultxt = ultxt
         endif else if frame eq n_frames-1 then begin
            contour_qbo, wind, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 0.,                                $
                         y2tickv = ltherma, y2tickname = salt,       $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         bltxt = filtxt, brtxt = inttxt
         endif else begin
            contour_qbo, wind, jd, therm, trange = t_range(frame,*), $
                         yrange = thermrange,                        $
                         ytickv = ltherm, ytickname = spress,        $
                         shade1 = 0.,                                $
                         _extra = options, hbar = hbar, vbar = vbar, $
                         y2tickv = ltherma, y2tickname = salt
         endelse
      endfor
   endelse
                
end

