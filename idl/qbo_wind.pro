; Make QBO wind for PhD thesis
;
; C. Marquardt
;

pro QBO_WIND, EPS = eps, LETTER = letter, TRANGE = trange, HBAR = hbar

;  add_path, '/home/marq/idl/qbo'
   add_path, '/home/aktuell/local/lib/idl/time_series'

   if n_elements(trange) eq 2 then begin
      ; latexinit, 18., 4, /color, file = 'figs/qbo_wind.eps'
      ; !p.charsize = 1.
      epsinit, 'figs/qbo_wind.eps'
      sdate = trange(0)
      edate = trange(1)
      compact = 0
      eps = 1
   endif else if keyword_set(eps) then begin
      epsinit, 'figs/qbo_wind.eps'
      ; latexinit, tex2cm(41, /pc), 23, /color, file = 'figs/qbo_wind.eps'
      ; !p.charsize = 0.75*2
      compact = 0
   endif else begin
      if keyword_set(letter) then begin
         psinit, /full, /color, /double, file = 'figs/qbo_wind_letter.ps', $
            comment = 'C. Marquardt and B. Naujokat, 1999'
      endif else begin
         psinit, /a4, /full, /color, /double, file = 'figs/qbo_wind.ps',  $
            comment = 'C. Marquardt and B. Naujokat, 1999'
      endelse
      !p.charsize = 1.75
      compact = 0
   endelse

   !x.margin = [5, 4]

   if n_elements(trange) eq 2 then begin
      qbo, startdate = sdate, enddate = edate ;, c_charsize = 0.75*!p.charsize
   endif else begin
      qbo, compact = compact;, hbar = (hbar/1000.)^(2./7.)
   endelse

   if keyword_set(eps) then begin
      latexterm
   endif else begin
      psterm, /noplot
   endelse

end
