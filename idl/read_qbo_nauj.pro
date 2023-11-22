; $Id$

function READ_QBO_NAUJ, LOW = low, HIGH = high, JOIN = join,        $
                        COUNT = count, JD = jdtime, JS = jstime,    $
                        ALTITUDE = altitude, PRESSURE = pressure,   $
                        THERMVAR = thermvar, NAN = nan,             $
                        DATAHOME = datahome
;+
; NAME:
;       READ_QBO_NAUJ
;
; PURPOSE:
;       This function reads monthly mean values of equatorial zonal
;       wind from the appropiate files of the Naujokat (86) data
;       set. Missing data is coded with the value 99.9 or NaN.
;
; CATEGORY:
;       Input/output.
;
; CALLING SEQUENCE:
;       DATA = READ_QBO()
; 
; INPUTS:
;       None.
;
; OPTIONAL INPUTS:
;       DATAHOME: Full path to base directory of data files; default
;                 is /home/marq/data/qbo/data.
;       
; KEYWORD PARAMETERS:
;       /NAN:    Set missing values to NaN.
;       /LOW:    Low vertical resolution data set only.
;       /HIGH:   High vertical resolution data set only. Default is to
;                use high resolution data when and where it is available.
;       /JOIN:   Use high resolution data set where available, low
;                resolution data set where necessary. This is the
;                default and needs not to be specified explicitely.
;
; OUTPUTS:
;       Datafield containing a time height series of monthly mean
;       zonal wind.
;
; OPTIONAL OUTPUTS:
;       COUNT:    Number of valid vertical profiles.
;       JD:       Array with Julian day number of each profile.
;       JS:       Array with Julian second of each profile (at 0:00).
;       ALTITUDE: Array with altitudes (in meter).
;       PRESSURE: Array with pressure levels (in hPa).
;       THERMVAR: Array with thermodynamic coordinate (i.e.,
;                 (p/p_s)^kappa, where p_s is surface pressure (1000 hPa) 
;                 and kappa = R/c_p (= 2/7).
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       Fields given as optional arguments are destroyed if they
;       existed previously.
;
; RESTRICTIONS:
;       No checking for non-existing files.
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;       To get the low resolution QBO data and obtain the dates in
;       Julian seconds, the altitude of all levels in the array alt,
;       the pressures in press, try
;
;          qbo_wind = read_qbo(/low, altitude = alt,  $
;                              pressure = press, js = dates)
;
;
; MODIFICATION HISTORY:
;
;       Thu Mar 7 12:56:54 1996, Christian Marquardt <marq@kassandra>
;
;		Added common block qbocom; reading raw data is now
;		required only once during each IDL session.
;
;       Wed Feb 14 13:19:24 1996, Christian Marquardt
;       <marq@strat06.met.fu-berlin.de>
;
;               Added support for high resolution database.
;
;       Mon Feb 12 10:43:21 1996, Christian Marquardt <marq@kassandra>
;
;               Added /NAN keyword.
;
;       Fri Jan 19 16:09:40 1996, Christian Marquardt <marq@kassandra>
;
;               Created.
;
;-

;  Common block containing already read data
;  -----------------------------------------

   common qbo_nauj_com, jdtime_l, jstime_l, press_l, data_l,  $
                        jdtime_h, jstime_h, press_h, data_h,  $
                        jdtime_j, jstime_j, press_j, data_j


;  Check arguments for low and/or high resolution database
;  -------------------------------------------------------

   if keyword_set(join) then begin
      low  = 1
      high = 1
   endif

   if (not keyword_set(low)) and (not keyword_set(high)) then begin
      low  = 1
      high = 1
      join = 1
   endif


;  Prepare filename base
;  ---------------------

   if n_elements(datahome) eq 0 then  $
      datahome = getenv('DATAHOME')
   if strlen(datahome) eq 0 then  $
      datahome = '/local/home/aktuell/data/qbo/data'

   filebase = datahome

;  Read data
;  ---------

;  Low resolution
;  --------------

   if ( keyword_set(low) and n_elements(data_l) eq 0 ) then begin

      press_l = [70., 50., 40., 30., 20., 15., 10.]

;     Parse file

      openr, lun, filebase + '/qbo.dat', /get_lun

      line = ''
      count = 0
      while (not eof(lun)) do begin
         readf, lun, line
         if isnumber(line(0)) ne 0 then begin
            count = count + 1
            reads, line, format = '(i5,i3,i2,i6,i2,5(i5,i2),i5)',  $
                   station, year, month,                           $
                   h70, dum, h50, dum, h40, dum, h30, dum,         $
                   h20, dum, h15, dum, h10
            year = year + 1900
            if count eq 1 then begin
               data_l = [h70, h50, h40, h30, h20, h15, h10] / 10.
               jdtime_l = ymd2jd(year, month, 15)
               jstime_l = ymds2js(year, month, 15, 0)
            endif else begin
               data_l = [ [[data_l]],  $
                          [[h70, h50, h40, h30, h20, h15, h10] / 10.]  $
                        ]
               jdtime_l = [jdtime_l, ymd2jd(year, month, 15)]
               jstime_l = [jstime_l, ymds2js(year, month, 15, 0)]
            endelse
         endif
      endwhile

      free_lun, lun

   endif

;
;  High resolution
;  ---------------
;

   if ( keyword_set(high) and n_elements(data_h) eq 0 ) then begin

      press_h = [90., 80., 70., 60., 50., 45., 40.,  $
                 35., 30., 25., 20., 15., 12., 10.]

;     Parse file

      openr, lun, filebase + '/qbo.highres.dat', /get_lun

      line = ''
      count = 0
      wind = fltarr(14)
      while (not eof(lun)) do begin
         readf, lun, line
         if isnumber(line(0)) ne 0 then begin
            count = count + 1
            reads, line, year, month, wind
            if count eq 1 then begin
               data_h = reverse(wind)
               jdtime_h = ymd2jd(year, month, 15)
               jstime_h = ymds2js(year, month, 15, 0)
            endif else begin
               data_h = [ [[data_h]],  $
                          [reverse(wind)]  $
                        ]
               jdtime_h = [jdtime_h, ymd2jd(year, month, 15)]
               jstime_h = [jstime_h, ymds2js(year, month, 15, 0)]
            endelse
         endif
      endwhile

      free_lun, lun

   endif

;
;  Joined data
;  -----------
;

   if ( keyword_set(join) and n_elements(data_j) eq 0 ) then begin

;     Find index where the high resolution data starts. It is assumed
;     that the high resolution database is complete, i.e. that no
;     profiles after the first one are missing.

      last_index = where(jdtime_l eq jdtime_h(0)) - 1
      last_index = last_index(0)

      if last_index eq -1 then begin
         print,  "Times (julian days) do not match."
         return, -1
      endif

;     Interpolate onto high resolution vertical grid in altitude. In
;     order to get a reasonable behaviour of spl_init2 and spl_interp2,
;     missing values are set to NaN first.

      alt_l = - 7000. * alog(press_l/1000.)
      alt_h = - 7000. * alog(press_h/1000.)

      data_l(where(data_l gt 90.)) = !values.f_nan

      data = transpose(                                                  $
               interp_qbo(transpose(data_l(*, 0:last_index)),            $
                          jdtime_l(0:last_index), alt_l, alt_h, /spline) $
             )
                              
;     Convert NaN's back to normal

      data_l(where(finite(data_l) eq 0)) = 99.9
      data(where(finite(data) eq 0)) = 99.9

;     Now, append the high resolution field

      data_j = [ [data], [data_h] ]

   endif

;
;  Altitude and pressure fields; also copy fields
;  ----------------------------------------------
;

   if keyword_set(join) then begin
      pressure = press_h
      jdtime = jdtime_l
      jstime = jstime_l
      data = data_j
   endif else begin
      if keyword_set(low) then begin
         pressure = press_l
         jdtime = jdtime_l
         jstime = jstime_l
         data = data_l
      endif else begin
         pressure = press_h
         jdtime = jdtime_h
         jstime = jstime_h
         data = data_h
      endelse
   endelse

   altitude = - 7000. * alog(pressure/1000.)
   thermvar = (pressure/1000.)^(2./7.)

;
;  Missing values -> NaN
;  ---------------------
;

   if keyword_set(nan) then begin
      nan_ind = where(data gt 90.)
      if nan_ind(0) ne -1 then  $
         data(nan_ind) = !values.f_nan
   endif

   return, transpose(data)

end
