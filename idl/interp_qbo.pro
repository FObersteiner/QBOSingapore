; $Id$

function INTERP_QBO, WIND, TIME, ALT, NEWALT, INTTXT = inttxt, $
                     LINEAR = linear, POLY = poly,             $
                     PORDER = porder, SPLINE = cubic_spline,   $
                     SPLINT = splint, TENSION = tension,       $
                     AKIMA = akima, DERIVATIVE = derivative

;+
; NAME:
;       INTERP_QBO
;
; PURPOSE:
;       This routine performs various interpolations on the vertical
;       profiles of zonal equatorial wind. On request, it returns the
;       derivatives of the interpolated profiles.
;
; CATEGORY:
;       Interpolation.
;
; CALLING SEQUENCE:
;       IWIND = INTERP_QBO(WIND,TIME,ALT,NEWALT,/<keyword>)
;
; INPUTS:
;       WIND:     2D array containing wind data (in time,altitude)
;       TIME:     1D vector containing dates (only used for looping
;                 over data).
;       ALT:      1D vector containing altitudes of wind data field.
;       NEWALT:   1D vector containing altitudes of new (interpolated)
;                 wind data field.
;
; OPTIONAL INPUTS:
;       PORDER:   Order of interpolating polynomial in case of
;                 /POLY. Default if 3.
;
; KEYWORD PARAMETERS:
;       /LINEAR:  Linear interpolation.
;       /POLY:    Polynomial interpolation of degree PORDER
;       /SPLINE:  Natural cubic spline interpolation.
;       /SPLINT:  Spline interpolation with tension.
;       /TENSION: Tension (default: 1.0).
;       /AKIMA:   Akima quasi cubic hermite spline interpolation.
;       /DERIVATIVE: Return first derivative.
;
; OUTPUTS:
;       IWIND:    2D array containing the interpolated wind data
;                 field, evaluated at the altitudes given in NEWALT,
;                 or the first derivative at the same points.
;
; OPTIONAL OUTPUTS:
;       INTTXT:   Short string describing the interpolation.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;       Loops over all time indices in case of 1d interpolation.
;       Requires the routines dydx from the ESRG IDL library.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;       Wed Feb 28 15:41:10 1996, Christian Marquardt
;       <marq@strat06.met.fu-berlin.de>
;
;		Created.
;
;-

;
;  Check for correct arguments
;  ---------------------------
;

   if not keyword_set(linear)       then linear       = 0
   if not keyword_set(poly)         then poly         = 0
   if not keyword_set(cubic_spline) then cubic_spline = 0
   if not keyword_set(splint)       then splint       = 0
   if not keyword_set(akima)        then akima        = 0

   if (linear + poly + cubic_spline + splint + akima) ne 1 then begin
      print,  "No interpolation method choosen; using linear."
      linear = 1
   endif

;
;  Interpolations...
;  -----------------
;

   n_times = n_elements(time)
   n_alt   = n_elements(alt)
   n_inter = n_elements(newalt)

   iwind = fltarr(n_times, n_inter)

;  Linear interpolation

   if keyword_set(linear) then begin
      inttxt = "linear"
      for index = 0, n_times-1 do begin
         iwind(index, *) = interp1(alt, wind(index, *), newalt)
      endfor
   endif

;  Polynomial interpolation (see Numerical Recipes, 2nd Ed.)

   if keyword_set(poly) then begin
      if n_elements(porder) ne 1 then porder = 3
      inttxt = "polynomial of order " + string(porder, format = '(i1)')
      for index = 0, n_times-1 do begin
         for ilevel = 0, n_inter-1 do begin
            level = where(alt le newalt(ilevel)) > 0
            level = level(n_elements(level)-1)
            k = min([max([level-porder/2, 0]), n_alt-1-porder])
            iwind(index, ilevel) =  $
                  fpolint(alt(k:*), wind(index, k:*), porder+1, newalt(ilevel))
         endfor
      endfor
   endif

;  Cubic spline interpolation

   if keyword_set(cubic_spline) then begin
      inttxt = "natural cubic spline"
      for index = 0, n_times-1 do begin
         coeff = spl_init2(alt, wind(index, *))
         iwind(index, *) = spl_interp2(alt, wind(index, *), coeff, newalt)
      endfor
   endif

;  Spline with tension

   if keyword_set(splint) then begin
      if n_elements(tension) ne 1 then tension = 1.0
      if tension ge 10. then  $
         format = '(f5.1)'  $
      else  $
         format = '(f5.2)'
      inttxt = "spline (tension" +  $
               string(tension, format = format) + $
               ")"
      for index = 0, n_times-1 do begin
         iwind(index, *) = spline(alt, wind(index, *), newalt, tension)
      endfor
   endif

;  Akima quasi cubic hermite spline (and derivative as well)

   if keyword_set(akima) then begin
      inttxt = "quasi-cubic hermite Akima spline"
      for index = 0, n_times-1 do begin
;         if keyword_set(derivative) then begin
;            coeff = akima_init(alt, wind(index, *))
;            iwind(index, *) = akima_interp(alt, wind(index, *), coeff,  $
;                                           newalt, /derivative)
;         endif else begin
            coeff = akima_init(alt, wind(index, *))
            iwind(index, *) = akima_interp(alt, wind(index, *), coeff,  $
                                           newalt)
;         endelse
      endfor
   endif

;  Calculate derivative

   if keyword_set(derivative) then begin
      for index = 0, n_times-1 do begin
         iwind(index, *) = dydx(newalt, iwind(index, *))
      endfor
   endif

   return, iwind

end
