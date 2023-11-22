program interpolate_qbo

  implicit none
  !
  ! global parameters
  !
  !
  ! logical unit numbers of input and output files
  integer, parameter :: nin  = 333
  integer, parameter :: nout = 334
  integer, parameter :: nerr = 335
  integer, parameter :: ntmp = 336
  !
  ! number of main levels
  integer, parameter :: nlev=21
  !
  ! main levels (Hauptflaechen)
  real, parameter, dimension(nlev) :: hlev = &
       (/ 99.9, 90.0, 80.0, 70.0, 60.0, 50.0, 45.0, &
       40.0, 35.0, 30.0, 25.0, 22.5, 20.0, 15.0, &
       12.0, 10.0,  8.0,  7.0,  6.0,  5.0,  4.0 /)
  !
  ! pi      - number PI
  ! faktor  - to convert knots to wind speed in m/s
  !
  real, parameter :: pi = 4.0*atan(1.0)
  real, parameter :: faktor = -18.52/36. ! -0.51444
  !
  ! data of the radiosonde
  !  p   - pressure
  !  u   - zonal wind 
  !  v   - meriodional wind
  !  x   - p**(R_d/c_p)  = p**0.286 
  !
  real, dimension(100) :: p, u, v, x
  !
  integer :: dd     ! wind direction sounding          (input)
  integer :: ffi    ! absolute wind strength in knots  (input)
  real    :: pp     ! pressure from sounding           (input)
  real    :: ff     ! ffi converted to floating point
  real    :: bmdd   ! wind direction in radians

  !
  real, dimension(nlev) :: hu, hv, hx, su, sv
  !
  character(len=100) :: infile, outfile, errfile, tmpfile
  character(len=100) :: line
  !
  integer :: yr, mo, dy, hr, h
  integer :: err
  integer :: anz, i
  !
  integer, dimension(31,2) :: daytab
  integer, dimension(nlev) :: counter
  !
  logical :: eof, sterm

  !Filenames einlesen--------------------------------------------
  read(*,'(a)') infile
  if(err/=0)then
     write(*,*) 'Fehler beim Einlesen der Inputdateibezeichnung'
     stop
  endif

  read(*,fmt='(a)',iostat=err) outfile
  if(err/=0)then
     write(*,*) 'Fehler beim Einlesen der Outputdateibezeichnung'
     stop
  endif

  read(*,fmt='(a)',iostat=err) errfile
  if(err/=0)then
     write(*,*) 'Fehler beim Einlesen der Errordateibezeichnung'
     stop
  endif

  read(*,fmt='(a)',iostat=err) tmpfile
  if(err/=0)then
     write(*,*) 'Fehler beim Einlesen der Tmpdateibezeichnung'
     stop
  endif
  !-------------------------------------------------------------


  !write(*,*) trim(infile)
  !Files Oeffnen -----------------------------------------------
  open(nin,file=trim(infile),iostat=err)
  if(err/=0)then
     write(*,*) 'Fehler beim Oeffnen der Inputdatei'
     stop
  endif

  open(nout,file=trim(outfile),iostat=err)
  if(err/=0)then
     write(*,*) 'Fehler beim Oeffnen der Outputdatei'
     stop
  endif

  open(nerr,file=trim(errfile),iostat=err)
  if(err/=0)then
     write(*,*) 'Fehler beim Oeffnen der Errordatei'
     stop
  endif

  open(ntmp,file=trim(tmpfile),iostat=err)
  if(err/=0)then
     write(*,*) 'Fehler beim Oeffnen derTMpdatei'
     stop
  endif

  !--------------------------------------------------------------

  eof     = .FALSE.
  daytab  = 0
  counter = 0
  su      = 0.
  sv      = 0.

  !logical um zu checken ob der erste Termin untersucht wird
  !
  sterm = .TRUE.

  !read infile until eof=true
  !
  !do while (eof == .FALSE.)
  do while (eof .eqv. .FALSE.)
     read (nin,'(a12)',iostat=err) line
     !write(*,*) line
     !
     if (err /= 0) then  ! End of Infile
        !
	!write(*,*) "End of Infile"
        !
	if (err==-1 .and. (.not.sterm)) then
           call printday (yr, mo, dy, hr, hlev, hx, hu, hv, &
                p, x, u, v, anz, su, sv, counter)
           !checken ob jeder Termin nur einmal dran war
           ! und dann die Ergebnisse printen
           !write(*,*) checkdaytable(daytab)
           if (checkdaytable(daytab)) then
              call printqbo (yr, mo, hlev, su, sv, counter)
              !write(*,*) 'alles gut'
           endif
	endif
        exit
     endif

     !write(*,*) line(1:1)
     if (line(1:1) == 'D') then
	!letzten termin auswerten
	if (.not.sterm) then
           call printday(yr, mo, dy, hr, hlev, hx, hu, hv, &
                p, x, u, v, anz, su, sv, counter)		
	endif
	sterm = .FALSE.
	read (line,'(1x,i4,i2,i2,1x,i2)',iostat=err) yr, mo, dy, hr

	if (err /= 0) then
           write (nerr,*) "Datumsformatierung falsch!"
           eof = .TRUE.
	else
    !write(*,'(a1,i4,i0.2,i0.2,i0.2)') "D",yr,mo,dy,hr
    !Termine mitzaehlen
           h = (hr/12) + 1
           daytab(dy,h) = daytab(dy,h) + 1
           !write(*,*) dy,hr,h
           !anzahl der Werte fuer den Termin 0 setzen
           anz = 0
           !Daten der Radiosonde
           p = 0.
           x = 0.
           u = 0.
           v = 0.
           !Werte der Hauptflaechen
           hu = 0.
           hv = 0.
           hx = 0.
	endif

     else 
	read (line,'(f5.1,1x,i3,1x,i2)',iostat=err) pp, dd, ffi
	if(err/=0)then
           write (nerr,'(1x,i4,i2,i2,1x,i2)') yr,mo,dy,hr
           write (nerr,*) "Winddaten falsch formatiert!"
           eof = .TRUE.
	endif
	ff = real(ffi)
 !
 ! Hier geht es jetzt weiter
 ! write(*,'(f5.1,1x,i3,1x,i2)') pp,dd,ff
 ! wenn die Windrichtung nicht durch 5 teilbar ist 
 ! (z.B. bei 336), dann ist der Wind in x+100kn angegeben
 !
	if (mod(dd,5) /= 0) then
           dd = dd - 1
           ff = ff + 100.
	endif
 !
 !wenn die Windrichtung groe"ser als 360° ist
 !
	if (dd > 360) then
           write (nerr,'(1x,i4,i2,i2,1x,i2)') yr,mo,dy,hr
           write (nerr,*) 'Windrichtung zu gro"s!'
           eof = .TRUE.
	else
           anz  = anz + 1
           bmdd = real((dd/180.0)*pi)
           !write(*,*) ff, faktor, ff*faktor
           ff     = ff * faktor
           u(anz) = ff * SIN(bmdd)
           v(anz) = ff * COS(bmdd)
           if (pp == 100.0) then
              pp = 99.9
           endif
           x(anz) = pp**0.286
           p(anz) = pp

           !write(*,'(6(f6.2,1x),i3)')u(anz),v(anz),x(anz),p(anz),ff,bmdd,dd
           !checken ob ein hauptlevel getroffen wurde
           do i = 1, nlev
              if ((hlev(i) == pp) .and. (hx(i) == 0)) then
                 hu(i) = u(anz)
                 hv(i) = v(anz)
                 hx(i) = x(anz)
                 exit
              endif
           end do

	endif
     endif
  end do

  !write(*,*) 'Zeichen:"',char(12),'"'

CONTAINS
  !
  ! ================================================================
  ! =========== SUBROUTINE SECTION =================================
  ! ================================================================
  !
  logical function checkdaytable(daytab)
    !
    integer, dimension(:,:), intent(in) :: daytab
    !
    integer :: i, j
    integer :: nd1, nd2
    !
    nd1 = size(daytab, 1)
    nd2 = size(daytab, 2)
    checkdaytable = .TRUE.
    !
    do i = 1, nd1
       do j = 1, nd2
          if (daytab(i,j)>1) then
             checkdaytable = .FALSE.
          endif
       end do
       !write(*,*)(daytab(i,j),j=1,2)
    end do
    return 
  end function checkdaytable
  !
  ! ===========================================
  !
  subroutine printqbo(yr, mo, hlev, su, sv, counter)
    !
    integer,               intent(in) :: yr, mo
    integer, dimension(:), intent(in) :: counter
    real,    dimension(:), intent(in) :: hlev
    real,    dimension(:), intent(in) :: su, sv
    !
    integer      :: i
    character(4) :: yy
    real,    dimension(nlev) :: mu,  mv
    integer, dimension(nlev) :: mdd, mff

    !write(*,*) counter
    do i = 1, nlev
       if (counter(i) /= 0) then
          mu(i) = su(i)/counter(i)
          mv(i) = sv(i)/counter(i)
          call vector(mu(i), mv(i), mdd(i), mff(i))
       else
          mu(i)  = 0.0
          mv(i)  = 0.0
          mdd(i) = 0
          mff(i) = 0
       endif
    end do
    write(yy,'(i4)')yr
    write(nout,'(/,a25,i3,i5,/)')  &
         'Monatsmittelwind fuer den ',mo,yr
    write(nout,'(a6,1x,a6,2(5x,a1),2x,2(1x,a2))') &
         'Niveau','Anzahl','U','V','DD','FF'
    write(nout,'(1x,a5,8x,2(a5,1x),4x,4a)')'[hPa]','[m/s]','[m/s]','[kn]'
    do i=1,21
       write(nout,'(f6.1,i7,2(f6.1),i5,i3)')  &
            hlev(i), counter(i), mu(i), mv(i), mdd(i), mff(i)
    end do
    write(ntmp,'(i4,1x,a5,1x,a2,i2.2,21(1x,i4))')  &
         yr, '48698', yy(3:4), mo, (nint(mu(i)*10), i=1,nlev)

  end subroutine printqbo
  !
  ! ===========================================
  !
  subroutine printday(yr, mo, dy, hr, hlev,   & ! INTENT(IN)
       hx, hu, hv, p, x, u, v, & ! INTENT(IN OUT)
       anz, su, sv, counter)     ! INTENT(IN OUT)

    integer,               intent(in) :: yr, mo, dy, hr
    real,    dimension(:), intent(in) :: hlev
    !
    integer,               intent(inout) :: anz
    real,    dimension(:), intent(inout) :: p, u, v, x
    real,    dimension(:), intent(inout) :: hu, hv, hx
    real,    dimension(:), intent(inout) :: su, sv
    integer, dimension(:), intent(inout) :: counter
    !
    integer :: i
    !
    if(anz==0) then
       write(nout,'(1x,i2,a1,i2,a1,i4,2x,i2,a4)') dy,'.',mo,'.',yr,hr,' Uhr'
       write(nout,'(/,1x,20(4(f4.1,2f6.1,3x),/,1x),/)') &
            (hlev(i), hu(i), hv(i), i=1,nlev)
       write(nout,*)
    else
       !write(*,*)'anz: ',anz
       call sort(p, x, u, v, anz)
       !write(*,*)'anz nach sort: ',anz
       call reduce(p, x, u, v, anz)
       !write(*,*)'anz nach reduce: ',anz
       write(nout,'(1x,i2,a1,i2,a1,i4,2x,i2,a4)') &
            dy,'.',mo,'.',yr,hr,' Uhr'
       write(nout,'(/,1x,20(4(f4.1,2f6.1,3x),/,1x))') &
            (p(i),u(i),v(i), i = 1, anz)
       call interpolate(hlev, hx, hu, hv, p, x, u, v, &
            anz, su, sv, counter)
       write(nout,'(/,1x,20(4(f4.1,2f6.1,3x),/,1x),/)') &
            (hlev(i),hu(i),hv(i),i = 1, nlev)
       write(nout,*)
    endif
    return
  end subroutine printday
  !
  ! ===========================================
  !
  subroutine interpolate(hlev, hx, hu, hv, p, x, u, v, &
       anz, su, sv, counter)
    !
    integer,               intent(in) :: anz
    real,    dimension(:), intent(in) :: p, u, v, x
    real,    dimension(:), intent(in) :: hlev
    !
    real,    dimension(:), intent(inout) :: hu, hv, hx, su, sv
    integer, dimension(:), intent(inout) :: counter

    integer :: i, j
    real    :: plow, phigh 
    real    :: ulow, uhigh
    real    :: vlow, vhigh
    real    :: xlow, xhigh
    real    :: tmp
    real    :: udiff, vdiff, xdiff
    real    :: xscale

    !write(*,*) 'interpolate u vektor'
    !write(*,*) hu
    do i = 1, nlev
       if (hx(i)==0.) then
          hx(i) = hlev(i)**0.286
          !write(*,*) 'hx: ',hx(i),' hlev: ',hlev(i)
          xlow  = 0.
          xhigh = 0.
          do j = 1, anz
             !
             ! die pxhigh und pxlow bestimmen
             !
             if (x(j) > hx(i)) then
                xlow = x(j)
                plow = p(j)
                ulow = u(j)
                vlow = v(j)
             else
                xhigh = x(j)
                phigh = p(j)
                uhigh = u(j)
                vhigh = v(j)
                exit
             endif
          enddo
          !write(*,*) 'xlow: ',xlow,' xhigh: ',xhigh
          !write(*,*)'plow: ', plow,' phigh: ',phigh
          !write(*,*) xlow,hx(i),xhigh,counter(i),plow,hlev(i),phigh
          !write(*,'(2(f15.10))') plow,phigh,hlev(i)*1.033,hlev(i)*0.967
          if (xlow==0. .and. xhigh==0.) then
             exit
          elseif (xlow/=0. .and. xhigh/=0.) then
             udiff  = uhigh - ulow
             vdiff  = vhigh - vlow
             xdiff  = xhigh - xlow
             xscale = xhigh - hx(i)
             tmp    = (udiff/xdiff) * xscale
             hu(i)  = uhigh - tmp
             tmp    = (vdiff/xdiff) * xscale
             hv(i)  = vhigh - tmp
             su(i)  = su(i) + hu(i)
             sv(i)  = sv(i) + hv(i)
             counter(i) = counter(i) + 1

          elseif ((xlow==0) .and. (phigh>=hlev(i)*0.967)) then
             !
             ! falls es keinen unteren wert gibt aber der 
             ! obere im 3% bereich des hauptlevels ist
             !
             udiff  = u(2) - u(1)
             vdiff  = v(2) - v(1)
             xdiff  = x(2) - x(1)
             xscale = x(2) - hx(i)
             tmp    = (udiff/xdiff) * xscale
             hu(i)  = u(2) - tmp
             tmp    = (vdiff/xdiff) * xscale
             hv(i)  = v(2) - tmp
             su(i)  = su(i) + hu(i)
             sv(i)  = sv(i) + hv(i)
             counter(i) = counter(i) + 1
          elseif ((xhigh==0) .and. (plow<=hlev(i)*1.033)) then
             !
             ! falls es keinen oberen wert gibt aber der 
             ! untere im 3% bereich des hauptlevels ist
             !
             udiff  = u(anz) - u(anz-1)
             vdiff  = v(anz) - v(anz-1)
             xdiff  = x(anz) - x(anz-1)
             xscale = x(anz) - hx(i)
             tmp    = (udiff/xdiff) * xscale
             !write(*,*)'u(n): ',u(anz),' u(n-1): ',u(anz-1)
             !write(*,*)'x(n): ',x(anz),' x(n-1): ',x(anz-1)
             !write(*,*)'xscale: ',xscale
             !write(*,*)'tmp: ',tmp
             hu(i) = u(anz) - tmp
             tmp   = (vdiff/xdiff) * xscale
             hv(i) = v(anz) - tmp
             su(i) = su(i) + hu(i)
             sv(i) = sv(i) + hv(i)
             counter(i)=counter(i) + 1
          endif
       else
          su(i) =su(i) + hu(i)
          sv(i) =sv(i) + hv(i)
          counter(i) = counter(i) + 1
       endif
    end do
    !write(*,*) 'done'
    !write(*,*) hu
    return
  end subroutine interpolate
  !
  ! ===========================================
  !
  subroutine vector(u, v, d, f)
    !
    real,    intent(in)  :: u, v
    integer, intent(out) :: d, f
    !
    real :: tdd, angle, tff

    tff   = SQRT(u**2+v**2)
    angle = ASIN(u/tff)
    tdd   = (angle*180.0)/pi
    !write(*,*) tdd,tdd+180
    if (v < 0) then
       tdd = 180. - tdd
    endif
    d = tdd + 180. + 0.5
    if (d >= 360) then
       d = d - 360.
    endif
    f = tff / (faktor * (-1.0)) + 0.5
    return
  end subroutine vector
  !
  ! ===========================================
  !
  subroutine sort(p, x, u, v, anz)
    !
    ! sortiert die winddaten in absteigender 
    ! reihenfolge nach dem drucklevel
    !
    integer,            intent(in)    :: anz
    real, dimension(:), intent(inout) :: p, u, v, x

    integer :: ende
    integer :: start
    integer :: i, j
    integer :: pos
    real    :: xmax
    real    :: tp, tx, tu, tv

    ende = anz-1
    do i = 1, ende
       xmax  = x(i)
       pos   = i
       start = i + 1
       do j = start, ende+1
          ! suche das groe"ste Element
          if (x(j)>xmax) then
             xmax = x(j)
             pos  = j
          endif
       end do
       if (pos /= i) then
          !werte switchen
          tp = p(pos)
          tx = x(pos)
          tu = u(pos)
          tv = v(pos)

          p(pos) = p(i)
          x(pos) = x(i)
          u(pos) = u(i)
          v(pos) = v(i)

          p(i) = tp
          x(i) = tx
          u(i) = tu
          v(i) = tv
       end if
    end do
    return
  end subroutine sort
  !
  ! ===========================================
  !
  subroutine reduce(p, x, u, v, anz)
    !
    !loescht doppelte Eintraege
    !
    integer,            intent(inout) :: anz
    real, dimension(:), intent(inout) :: p, u, v, x
    !
    integer :: ende
    integer :: i,j

    ende = anz

    !write(*,*)p(1:ende+1)
    do i = 1, anz-1
       if (i >= ende) then
          exit
       else
          !elemente mit folgeelement verlgleichen
          if (p(i) == p(i+1)) then
             !write(*,*) i,p(i),p(i+1)
             do j = i+1, ende-1
                p(j) = p(j+1)
                x(j) = x(j+1)
                u(j) = u(j+1)
                v(j) = v(j+1)
             end do
             ende = ende-1
          end if
       endif
    end do
    anz = ende
    return
  end subroutine reduce

end program interpolate_qbo
