; $Id$

function FILTER_QBO, DATA, FILTER = filter, SAVGOL = savgol, FILTXT = filtxt

;+
; NAME:
;       FILTER_QBO
;
; PURPOSE:
;       This routine filters QBO wind time series with a running
;       weighted mean or a Savitzky-Golay filter.
;
; CATEGORY:
;       Filtering.
;
; CALLING SEQUENCE:
; 
; INPUTS:
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
;       None.
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
;       Sun Mar 3 14:30:13 1996, Christian Marquardt <marq@kassandra>
;
;		Created.
;
;-

;
;  Filter or smooth if necessary
;  -----------------------------
;

   filtxt = ''

   if n_elements(filter) ne 0 then begin

      print, "Filtering with window:", filter

      filtxt = "filter" +  $
                string(format="(' [',i2,',',i2,',',i2,'] ')",filter)

      size_data = size(data)
      n_levels = size_data(2)
      for level = 0, n_levels - 1 do begin
         data(*, level) =  $
           convol(data(*, level), filter, total(filter), /edge_truncate)
      endfor

   endif

   if n_elements(savgol) ne 0 then begin

      print, "Smoothing with Savitzky-Golay:", savgol

      if n_elements(savgol) eq 1 then begin
         savgol = [5., 2.]
      endif

      if n_elements(savgol) eq 2 then begin
         filtxt = "Savitzky-Golay" +  $
                   string(format="(' [',i2,',',i2,'] ')",savgol)
      endif else begin
         filtxt = "Savitzky-Golay" +  $
                   string(format="(' [',i2,',',i2,',',i2,'] ')",savgol)
      endelse

      size_data = size(data)
      n_levels = size_data(2)
      for level = 0, n_levels - 1 do begin
         if n_elements(savgol) ne 3 then begin
            dummy = poly_smooth(data(*, level), savgol(0),  $
                                degree = savgol(1), coeff = filter)
            data(*, level) = convol(data(*, level), filter, /edge_truncate)
         endif else begin
            dummy = poly_smooth(data(*, level), nleft = savgol(0),    $
                                nright = savgol(1), degree = savgol(2), $
                                coeff = filter)
            data(*, level) = convol(data(*, level), filter, /edge_truncate)
         endelse
      endfor

   endif

   return, data
end
