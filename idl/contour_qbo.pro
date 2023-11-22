; $Id$

pro CONTOUR_QBO, FIELD, TIME, VERT, TITLE = title,  SUBTITLE = subtitle,  $
                 LFILL = lfill, LEVELS = levels, TRANGE = trange,         $
                 XMARGIN = xmargin, XTICKLEN = xticklen,                  $
                 YMARGIN = ymargin, YTICKLEN = yticklen,                  $
                 YRANGE = yrange, YTICKV = ytickv, YTICKNAME = ytickname, $
                 YTITLE = ytitle,                                         $
                 Y2TICKV = y2tickv, Y2TICKNAME = y2tickname,              $
                 Y2TITLE = y2title,                                       $
                 SHADE1 = shade1, COLOR1 = color1,                        $
                 SHADE2 = shade2, COLOR2 = color2,                        $
                 CHARSIZE = charsize, C_CHARSIZE = c_charsize,            $
                 C_ORIENTATION = c_orientation,                           $
                 C_SPACING = c_spacing,                                   $
                 MAX_VALUE = max_value, MIN_VALUE = min_value,            $
                 ULTXT = ultxt, UCTXT = uctxt, URTXT = urtxt,             $
                 LLTXT = lltxt, LCTXT = lctxt, LRTXT = lrtxt,             $
                 ALTXT = altxt, ACTXT = actxt, ARTXT = artxt,             $
                 BLTXT = bltxt, BCTXT = bctxt, BRTXT = brtxt,             $
                 HBAR = hbar, VBAR = vbar, COLOR3 = color3,               $
                 _EXTRA = extra

;+
; NAME:
;       CONTOUR_QBO
;
; PURPOSE:
;       This routine contours (and fills) a 2D dataset in a QBO-like
;       fashion. 
;
; CATEGORY:
;       Graphics.
;
; CALLING SEQUENCE:
; 
; INPUTS:
;       FIELD: 2D field with (time,height) values.
;       TIME:  Time values of individual profiles (in Julian days).
;       VERT:  Vertical coordinate values for individual levels.
;
; OPTIONAL INPUTS:
;	LEVELS: Vector containing values of contour levels.
;       SHADE1: Scalar defining first value for shading.
;       SHADE2: Scalar defining second (negative) value for shading.
;       COLOR1: Color index for first shading.
;       COLOR2: Color index for second shading.
;
; KEYWORD PARAMETERS:
;       /LFILL: Fill with lines, not with color
;
; OUTPUTS:
;       None. Just a picture.
;
; OPTIONAL OUTPUTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       Draws a picture.
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;
;-

;
;  Arguments
;  ---------
;

   if n_params() ne 3 then begin
      print, "Hey, we need (at least) three args: field, time, and vert!"
      return
   endif

;
;  Default levels and shades
;  -------------------------
;

   if n_elements(levels) eq 0 then  $
      levels = -50. + indgen(11)*10.

   if n_elements(shade1) eq 0 then  $
      shade1 = 0.

;
;  Default options
;  ---------------
;

   if !d.name eq 'PS' then begin

      if n_elements(xmargin) eq 0 then xmargin = [6, 6]
      if n_elements(xticklen) eq 0 then xticklen = 1. ; 0.02

      if n_elements(ymargin) eq 0 then ymargin = [2, 1]
      if n_elements(yticklen) eq 0 then yticklen = 1.

      if (n_elements(charsize) eq 0) and (!p.charsize eq 0.) then $
          charsize = !p.charsize  $
      else                        $
          charsize = !p.charsize
      if n_elements(c_charsize) eq 0 then c_charsize = 0.75

      if n_elements(c_spacing) eq 0 then c_spacing = 0.075

   endif else begin

      if n_elements(xmargin) eq 0 then xmargin = [7, 7]
      if n_elements(xticklen) eq 0 then xticklen = 1. ; 0.02

      if n_elements(ymargin) eq 0 then ymargin = [4, 1]
      if n_elements(yticklen) eq 0 then yticklen = 1.

      if (n_elements(charsize) eq 0) and (!p.charsize eq 0.) then $
          charsize = 1.75  $
      else                 $
          charsize = !p.charsize
      if n_elements(c_charsize) eq 0 then c_charsize = 0.75

      if n_elements(c_spacing) eq 0 then c_spacing = 0.1

   endelse

   if n_elements(title) eq 0 then  $
      title = ''

   if n_elements(subtitle) eq 0 then $
      subtitle = ''

   if n_elements(trange) ne 2 then  $
      trange = [time(0), time(n_elements(time)-1)]

   if n_elements(yrange) ne 2 then  $
      yrange = [vert(0), vert(n_elements(vert) - 1)]

   if n_elements(ytitle) eq 0 then ytitle = 'hPa'

   if n_elements(y2title) eq 0 then y2title = 'km'

   if n_elements(c_orientation) eq 0 then c_orientation = 45.

   if n_elements(color1) eq 0 then color1 = 180
   if n_elements(color2) eq 0 then color2 = 130

   if n_elements(max_value) eq 0 then max_value = 99.
   if n_elements(min_value) eq 0 then min_value = -99.

;  Prepare additional texts on plot

   if n_elements(ultxt) eq 0 then ultxt = ''
   if n_elements(uctxt) eq 0 then uctxt = ''
   if n_elements(urtxt) eq 0 then urtxt = ''
   if n_elements(lltxt) eq 0 then lltxt = ''
   if n_elements(lctxt) eq 0 then lctxt = ''
   if n_elements(lrtxt) eq 0 then lrtxt = ''
   if n_elements(altxt) eq 0 then altxt = ''
   if n_elements(actxt) eq 0 then actxt = ''
   if n_elements(artxt) eq 0 then artxt = ''
   if n_elements(bltxt) eq 0 then bltxt = ''
   if n_elements(bctxt) eq 0 then bctxt = ''
   if n_elements(brtxt) eq 0 then brtxt = ''

;
;  Prepare y axis
;  --------------
;

   if n_elements(ytickv) gt 1 then    $
      yticks =  n_elements(ytickv) -1 $
   else begin
      ytickv(0) = 0.
      ytickv(1) = 0.
   endelse

;
;  Prepare secondary y axis
;  ------------------------
;

   if n_elements(y2tickv) gt 1 then     $
      y2ticks =  n_elements(y2tickv) -1 $
   else begin
      y2tickv(0) = 0.
      y2tickv(1) = 0.
   endelse

   if n_elements(y2tickv) eq 0 then begin
      y2tickv = ytickv
      y2ticks = yticks
      y2title = ytitle
   endif

;
;  Prepare time axis
;  -----------------
;

;  Calculate trange; make sure it starts in first of month...

   jd2ymd, trange(0), startyear, month, day
   if day ne 1 then  $
      trange(0) = ymd2jd(startyear, month, 1)

;  ...and ends on last of month.

   jd2ymd, trange(1), endyear, month, day
   if day ne monthdays(endyear, month) then  $
      trange(1) = ymd2jd(endyear, month, monthdays(endyear, month))

;  Major ticks for grid at beginning of each year

   syear = startyear

   tgtickv = ymd2jd(syear, 1, 1)
   if tgtickv lt trange(0) then begin
      syear = syear + 1
      tgtickv = ymd2jd(syear, 1, 1)
   endif
   tgtickname = ' '

   for year = syear + 1, endyear do begin
      tickv = ymd2jd(year, 1, 1)
      if tickv le trange(1) then begin
         tgtickv = [tgtickv, tickv]
         tgtickname = [tgtickname, ' ']
      endif
   endfor

   tgticks = n_elements(tgtickv) - 1

;  Major ticks for labels / annotation of years

   syear = startyear

   tltickv = ymd2jd(syear, 7, 1)
   if tltickv lt trange(0) then begin
      syear = syear + 1
      tltickv = ymd2jd(syear, 7, 1)
   endif
   tltickname = jd2date(tltickv, form = 'Y$')

   for year = syear + 1, endyear do begin
      tickv = ymd2jd(year, 7, 1)
      if tickv le trange(1) then begin
         tltickv = [tltickv, tickv]
         tltickname = [tltickname, jd2date(tickv, form = 'Y$')]
      endif
   endfor

   tlticks = n_elements(tltickv) - 1

;
;  Plot field
;  ----------
;

;  Mirror missing values (which are set a large POSITIVE number to
;  negative side of values so that contour, /fill is able to fill
;  areas given in shade1.

   cs = where(field ge max_value)
   if cs(0) ne -1 then field(cs) = - field(cs)

;  Plot shades with axis indicating months and year 'boundaries'

   if keyword_set(lfill) then begin
      contour, field, time, vert, title = title, subtitle = subtitle, $
               levels = shade1, /fill,                                $
               xmargin = xmargin, xrange = trange, xstyle = 1,        $
               xminor = 12, xticklen = xticklen, xtickv = tgtickv,    $
               xticks = tgticks, xtickname = tgtickname,              $
               xgridstyle = 1,                                        $
               ymargin = ymargin, yrange = yrange, ystyle = 1,        $
               ytitle = ytitle, yticklen = yticklen, ytickv = ytickv, $
               yticks = yticks, ytickname = ytickname, ygridstyle = 1,$
               c_orientation = 45., c_spacing = c_spacing,            $
               charsize = charsize, _extra = extra
   endif else begin
      contour, field, time, vert, title = title, subtitle = subtitle, $
               levels = shade1, /fill,                                $
               xmargin = xmargin, xrange = trange, xstyle = 1,        $
               xminor = 12, xticklen = xticklen, xtickv = tgtickv,    $
               xticks = tgticks, xtickname = tgtickname,              $
               xgridstyle = 1,                                        $
               ymargin = ymargin, yrange = yrange, ystyle = 1,        $
               ytitle = ytitle, yticklen = yticklen, ytickv = ytickv, $
               yticks = yticks, ytickname = ytickname, ygridstyle = 1,$
               c_colors = color1,                                     $
               charsize = charsize, _extra = extra
   endelse

;  Revert missing value mirroring

   if cs(0) ne -1 then field(cs) = - field(cs)

;  Do the same stuff for a second region to shade (if given)

   if n_elements(shade2) gt 0 then begin

      if keyword_set(lfill) then begin
         contour, -field, time, vert, levels = -shade2, /follow, /fill,  $
                  xmargin = xmargin, xrange = trange, xstyle = 1,        $
                  xminor = 12, xticklen = xticklen, xtickv = tgtickv,    $
                  xticks = tgticks, xtickname = tgtickname,              $
                  xgridstyle = 1,                                        $
                  ymargin = ymargin, yrange = yrange, ystyle = 1,        $
                  ytitle = ytitle, yticklen = yticklen, ytickv = ytickv, $
                  yticks = yticks, ytickname = ytickname, ygridstyle = 1,$
                  c_orientation = -45., c_spacing = c_spacing,           $
                  charsize = charsize, _extra = extra,                   $
                  /overplot
      endif else begin
         contour, -field, time, vert, levels = -shade2, /follow, /fill,  $
                  xmargin = xmargin, xrange = trange, xstyle = 1,        $
                  xminor = 12, xticklen = xticklen, xtickv = tgtickv,    $
                  xticks = tgticks, xtickname = tgtickname,              $
                  xgridstyle = 1,                                        $
                  ymargin = ymargin, yrange = yrange, ystyle = 1,        $
                  ytitle = ytitle, yticklen = yticklen, ytickv = ytickv, $
                  yticks = yticks, ytickname = ytickname, ygridstyle = 1,$
                  c_colors = color2,                                     $
                  /overplot
      endelse

;  Redraw axis

      axis, xaxis = 0, xrange = trange, xstyle = 1, xminor = 12,      $
            xticklen = xticklen, xtickv = tgtickv, xticks = tgticks,  $
            xtickname = tgtickname, xgridstyle = 1
      axis, xaxis = 1, xrange = trange, xstyle = 1, xminor = 12,      $
            xticklen = xticklen, xtickv = tgtickv, xticks = tgticks,  $
            xtickname = tgtickname, xgridstyle = 1
      axis, yaxis = 0, ymargin = ymargin, yrange = yrange, ystyle = 1, $
            ytitle = ytitle, yticklen = yticklen, ytickv = ytickv,     $
            yticks = yticks, ytickname = ytickname, ygridstyle = 1,    $
            charsize = charsize


   endif

;  Add axis and labels for years

   axis, xaxis = 0, xrange = trange, xstyle = 1, xminor = 0,     $
            xticklen = 0., xtickv = tltickv, xticks = tlticks,   $
            xtickname = tltickname, charsize = charsize

;  Add secondary y-axis

   axis, yaxis = 1, ymargin = ymargin, yrange = yrange, ystyle = 1, $
            ytitle = y2title, yticklen = 0.005, ytickv = y2tickv,   $
            yticks = y2ticks, ytickname = y2tickname,               $
            charsize = charsize

;  Plot contour lines on top

   contour, field, time, vert, levels = levels, /follow,  $
            xstyle = 1, xrange = trange, ystyle = 1, yrange = yrange, $
            c_charsize = c_charsize, max_value = max_value, $
            min_value = min_value, _extra = extra, /overplot
   contour, field, time, vert, levels = [0.], /follow,            $
            c_charsize = c_charsize, max_value = max_value,       $
            min_value = min_value, _extra = extra, c_thick = 2.,  $
            /overplot


;  Add a horizontal colored bar

   if n_elements(hbar) eq 2 then begin
      if n_elements(color3) ne 1 then begin
         color3 = 25
         color, 'red', color3
      endif
      x = [trange(0), trange(1), trange(1), trange(0)]
      y = [hbar(0), hbar(0), hbar(1), hbar(1)]
      polyfill, x, y, color = color3, orientation = 45., spacing = 0.1,  $
         /line_fill
   endif


;  Add a vertical colored bar

   if n_elements(vbar) eq 2 then begin
      if n_elements(color3) ne 1 then begin
         color3 = 25
         color, 'red', color3
      endif
      x = [vbar(0), vbar(0), vbar(1), vbar(1)]
      y = [yrange(0), yrange(1), yrange(1), yrange(0)]
      polyfill, x, y, color = color3, orientation = 45., spacing = 0.1,  $
         /line_fill
   endif


;  Add texts

   add_txt, ultxt = ultxt, uctxt = uctxt, urtxt = urtxt,  $
            lltxt = lltxt, lctxt = lctxt, lrtxt = lrtxt,  $
            altxt = altxt, actxt = actxt, artxt = artxt,  $
            bltxt = bltxt, bctxt = bctxt, brtxt = brtxt,  $
            charsize = 0.5*charsize

end
