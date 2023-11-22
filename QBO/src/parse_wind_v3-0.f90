program parse_wind

  ! Fortran 90 Portierung von singa2.f von Naujokat   
  ! Stefan Metzner FU Berlin 2012
  ! Markus Kunze   FU Berlin 2021
  !   - reorganised decoding of part A, C, D
  !   - added subroutines readin_part_a, readin_part_c
  !                       readin_part_d

  implicit none

  character(256) :: infile
  character(256) :: windfile
  character(256) :: errfile

  !instantane Werte fuer Richtung, Geschwindigkeit, Druckflaeche
  integer             :: dd,ff,pp

  !Verschiedene Integer
  integer        :: err,nin,nout,nerr,lln,lln2,lln3
  !
  character(100)              :: datestring
  character(100)              :: line,line2,line3
  character(3),dimension(5)   :: tmp_pp,tmp_dd,tmp_ff
  integer,     dimension(100) :: counter
  !
  !Datenfelder fuer Richtung, Geschwindigkeit, Druck
  integer, dimension(100) :: d,f
  real,    dimension(100) :: p
  !
  !Format
  character(50) :: fmt
  !
  integer :: idx, i, ic, c
  integer :: fdcount    !file delimiter counter
  !
  logical :: tta, ttc, ttd
  logical :: debug = .false.
  logical :: part_c_in = .false.
  logical :: part_d_in = .false.

  nin  = 333
  nout = 334
  nerr = 335

  !Filenames einlesen
  read(*,'(a)',iostat=err) infile
  if(err/=0)then
     write(*,*) 'Fehler beim Einlesen der Inputdateibezeichnung'
     call exit(err)
  endif

  read(*,fmt='(a)',iostat=err) windfile
  if(err/=0)then
     write(*,*) 'Fehler beim Einlesen der Outputdateibezeichnung'
     call exit(err)
  endif

  read(*,fmt='(a)',iostat=err) errfile
  if(err/=0)then
     write(*,*) 'Fehler beim Einlesen der Errordateibezeichnung'
     call exit(err)
  endif


  !Files oeffnen ------------------------------------------------
  open(nin,file=trim(infile),iostat=err)
  if(err/=0)then
     write(*,*) 'Fehler beim Oeffnen der Inputdatei'
     call exit(err)
  endif

  open(nout,file=trim(windfile),iostat=err)
  if(err/=0)then
     write(*,*) 'Fehler beim Oeffnen der Outputdatei'
     call exit(err)
  endif

  open(nerr,file=trim(errfile),iostat=err,position='append')
  if(err/=0)then
     write(*,*) 'Fehler beim Oeffnen der Outputdatei'
     call exit(err)
  endif
  !---------------------------------------------------------------

  !Flaechenzaehler 0 setzen und Datenfelder initialisieren
  idx=0
  p=-999
  d=-999
  f=-999

  counter=0
  fdcount=0

54 FORMAT(a54)
71 FORMAT(a71)

  !Datumsstring uebertragen----------------------------------------
  read(nin,'(4x,a11)') datestring
  !write(*,'(55a)') '-------------------------------------------------------' 
  !write(*,*) trim(datestring)
  !write(*,'(55a)') '-------------------------------------------------------' 
  write(nout,'(a12)') 'D'//trim(datestring)
  !----------------------------------------------------------------
  !A Teil Ueberpuefen----------------------------------------------
  !hier muss jetzt jeder Teil Ueberpueft werden
  tta=.false.
  read(nin,71,iostat=err) line
  if(err/=0)then
     write(nerr,*) 'D'//trim(datestring)
     write(nerr,*) 'A-Teil: Keine Daten und kein File-Delimiter'
     !write(*,*) 'A-Teil: Keine Daten und kein File-Delimiter'
     call exit(err)
  endif
  !write(*,*)len_trim(line),trim(line)
  if ((line(1:4) == 'TTAA')  .and. checkdelimiter(line)) then
  else if (len_trim(line)==1 .and. checkdelimiter(line)) then
     fdcount=1
  else if (line(1:4) == 'TTAA') then
     !write(*,*) 'A-Teil'
     
     call readin_part_a(nin, idx, d, f, p, tta)
     if (.not. tta) then
        write(nerr,*) 'D'//trim(datestring)
        write(nerr,*) 'A-Teil: Format fehlerhaft'
        !call exit(1)
     end if
     backspace(nin)
  else
     write(nerr,*) 'D'//trim(datestring)
     write(nerr,*) 'A-Teil: Keine Daten und kein File-Delimiter'
     !write(*,*) 'A-Teil: Keine Daten und kein File-Delimiter'
     call exit(err)
  endif
  !-----------------------------------------------------------------

  !zum C-Teil vorspulen---------------------------------------------
  !read(nin,71) line
  do while (line(1:4) /= 'TTCC' .and. (.not.checkdelimiter(line)))
     read(nin,71,iostat=err) line
     if (err /= 0) then	
        write (nerr,*) 'D'//trim(datestring)
        write (nerr,*) 'C-Teil: Keine Daten und kein File-Delimiter'
        part_c_in = .false.
        exit
     else
        part_c_in = .true.
     end if
     !write(*,*) 'C-Teil: Keine Daten und kein File-Delimiter'
     !write (*,*) len_trim(line),trim(line)
     !write (*,71) trim(line)
  enddo
  if (line(1:4) == 'TTCC') then
     backspace(nin)
  else if (checkdelimiter(line)) then
     fdcount=1
  endif
  !-----------------------------------------------------------------
  
  if (part_c_in) then
     !C Teil Ueberpruefen----------------------------------------------
     ttc=.false.
     read(nin,71,iostat=err) line
     if(err/=0)then
        write(nerr,*) 'D'//trim(datestring)
        write(nerr,*) 'C-Teil: Keine Daten und kein File-Delimiter'
        !write(*,*) 'C-Teil: Keine Daten und kein File-Delimiter'
        call exit(err)
     endif
     !write (*,*) len_trim(line),trim(line)
     if ((line(1:4) == 'TTCC') .and. (checkdelimiter(line))) then
        !write(*,*) 'D'//trim(datestring)
        !write(*,*) 'TTCC+delimiter'
        fdcount=fdcount+1
     else if (len_trim(line)==1 .and. checkdelimiter(line)) then
        fdcount=fdcount+1
     else if (line(1:4) == 'TTCC') then
        !write(*,*) 'C-Teil'
        call readin_part_c(nin, idx, d, f, p, ttc)
        if (.not. ttc) then
           write(nerr,*) 'D'//trim(datestring)
           write(nerr,*) 'C-Teil: Format fehlerhaft'
           !call exit(1)
        else
           fdcount=fdcount+1
        end if
        backspace(nin)
     else
        write (nerr,*) 'D'//trim(datestring)
        write (nerr,*) 'C-Teil: Keine Daten und kein File-Delimiter'
        !write(*,*) 'C-Teil: Keine Daten und kein File-Delimiter'
        call exit(err)
     end if
  end if
  !-----------------------------------------------------------------


  !zum D-Teil vorspulen---------------------------------------------
  !write (*,71) trim(line)
  do while (line(1:4) /= 'TTDD' .and. &
           (.not.checkdelimiter(line)))
     read (nin,71,iostat=err) line
     if (err /= 0) exit
     !write (*,*) len_trim(line),trim(line)
     !write (*,71) trim(line)
  enddo
  if (line(1:4)=='TTDD') then
     backspace(nin)
  endif
  !--------------------------------------------------------------------------------------


  !D Teil Ueberpruefen---------------------------------------------------------------------
  ttd = .false.
  read (nin,71,iostat=err) line
  if (err/=0) then
     write (nerr,*) 'D'//trim(datestring)
     write (nerr,*) 'D-Teil: Keine Daten und kein File-Delimiter'
     !write(*,*) 'D-Teil: Keine Daten und kein File-Delimiter'
     call exit(err)
  endif
  !write (*,*) trim(line)
  if ((line(1:4)=='TTDD') .and. &
       (checkdelimiter(line))) then
  elseif (checkdelimiter(line)) then
  elseif (line(1:4)=='TTDD') then
     !write(*,*) 'D-Teil'
     do while(line(1:5)/='21212')
        read(nin,71,iostat=err) line
        if (err /= 0) exit
        if(checkdelimiter(line)) call exit(err)
     enddo
     if (err == 0) then
        backspace(nin)
        call readin_part_d(nin, idx, d, f, p)
        ttd = .true.
     end if
  end if
  !--------------------------------------------------------------------------------------


  if(idx/=0)then
     !write(*,'(55a)') '-------------------------------------------------------' 
     !write(*,'(6a)') 'Daten:'
     do i=1,idx
        !write(*,'(f5.1,1x,i3,1x,i2)')p(i),d(i),f(i)
     end do
  elseif(idx==0)then
     !write(*,'(55a)') '-------------------------------------------------------' 
     !write(*,'(1x,22a)') 'Keine Daten vorhanden!'
  endif
  !write(*,'(55a)') '-------------------------------------------------------' 
  !write(*,*)
  do i=1,idx
     write(nout,'(f5.1,1x,i3,1x,i2)')p(i),d(i),f(i)
  end do

  close(nin)
  close(nout)

contains


  logical function checkdelimiter(line)
    implicit none
    character(100),intent(in) ::line
    character(1)              ::zeichen
    integer                   ::length

    length=len_trim(line)
    zeichen=line(length:length)

    if(zeichen=='=')then
       checkdelimiter=.true.
    else
       checkdelimiter=.false.
    endif
    return
  end function checkdelimiter
  
  subroutine readin_part_a(nin, idx, dd_out, ff_out, pp_out, tta)
  
    ! Decode the Part A data. 
    ! Part A contains data on mandatory levels up to 100 hPa.
    ! For the purpose of the QBO, only 100 hPa data are decoded.

    implicit none
    
    integer,               intent(in)     :: nin
    integer,               intent(in out) :: idx
    integer, dimension(:), intent(in out) :: dd_out, ff_out
    real,    dimension(:), intent(in out) :: pp_out
    logical,               intent(in out) :: tta
  
    integer             :: ll, llall, ierr
    integer             :: np, ip, nrest, i
    integer             :: dd, ff, pp
    character(len=100)  :: one_line = ''
    character(len=1024) :: long_line = ''
    character(len=3), dimension(100) :: tmp_pp, tmp_dd, tmp_ff
    integer,          dimension(100) :: counter
    
    ! concatenate all lines of part A
    
    long_line = ''
    i = 0
    partA: do
       read(nin,'(a)', iostat=ierr) one_line
       if (one_line(1:2) == 'TT' .or. one_line(1:1) == '=') exit partA
       i = i + 1
       if (ierr /= 0) exit partA
       ll = len_trim(one_line)
       ! '99' -> surface data
       if (i == 1 .and. one_line(1:2) == '99' .and. checkdelimiter(line)) then
          long_line = one_line(1:ll-1)
          exit partA
       else if (i == 1 .and. one_line(1:2) == '99' .and. .not.checkdelimiter(line)) then
          long_line = one_line(1:ll)
       else if (checkdelimiter(line)) then
          long_line = trim(long_line)//' '//one_line(1:ll-1)
          exit partA
       else
          long_line = trim(long_line)//' '//one_line(1:ll)
       end if
    end do partA
    
    ! skip the first 36 characters, as there is no wind data decoded
    
    long_line = long_line(37:)
    llall = len_trim(long_line)
    np    = llall/18
    nrest = mod(llall,18)
    
    if (debug) then
       write(*,'(a)') trim(long_line)
       write(*,*) 'A1: llall = ',llall,', np = ',np, &
                  ', nrest = ',nrest,' datestring:',trim(datestring)
    end if

110 FORMAT(a2,10x,a3,a2)
111 FORMAT(a2)
112 FORMAT('Part A: '6x,f5.1' hPa',1x,i3'°',1x,i2' kt ',a)

    ! now scan the long line and look for the 100 hPa level

    do ip = 1, np
       ll = len_trim(long_line)
       if (ll >= 18) then
          read(long_line,110) tmp_pp(ip), tmp_dd(ip), tmp_ff(ip)
          ! shift by 19, to reach the next pressure level
          long_line = long_line(19:)
          if (tmp_pp(ip) /= '//'  .and. &
              tmp_dd(ip) /= '///' .and. &
              tmp_ff(ip) /= '//') then
             read(tmp_pp(ip),'(i3)') pp
             read(tmp_dd(ip),'(i3)') dd
             read(tmp_ff(ip),'(i2)') ff
             if (pp == 10 .and. dd <= 360) then
                idx = idx + 1
                pp_out(idx) = pp*10.0
                dd_out(idx) = dd
                ff_out(idx) = ff
                tta = .true.
                write(*,112) pp_out(idx), dd_out(idx), ff_out(idx), trim(datestring)
             end if
          end if
       else
          read(long_line,111) tmp_pp(ip)
          if (debug) print *,'Part A: long_line = ',trim(long_line)
          if (tmp_pp(ip) == '88' .or. tmp_pp(ip) == '77') then
             tta = .true.
          end if
       end if
    end do
    return
  end subroutine readin_part_a
  
  subroutine readin_part_c(nin, idx, dd_out, ff_out, pp_out, ttc)
  
    ! Decode the Part C data. 
    ! Part C contains data on mandatory levels for pressures less than 100 hPa.
    ! These levels are 70, 50, 30, 20, and 10 hPa.

    implicit none
    
    integer,               intent(in)     :: nin
    integer,               intent(in out) :: idx
    integer, dimension(:), intent(in out) :: dd_out, ff_out
    real,    dimension(:), intent(in out) :: pp_out
    logical,               intent(in out) :: ttc
  
    integer             :: ll, llall, ierr
    integer             :: np, ip, nrest, i
    integer             :: dd, ff, pp
    character(len=100)  :: one_line = ''
    character(len=1024) :: long_line = ''
    character(len=3), dimension(100) :: tmp_pp, tmp_dd, tmp_ff
    integer,          dimension(100) :: counter
    
    ! concatenate all lines of part A
    
    long_line = ''
    i = 0
    partC: do
       read(nin,'(a)', iostat=ierr) one_line
       if (one_line(1:2) == 'TT' .or. one_line(1:1) == '=') exit partC
       i = i + 1
       if (ierr /= 0) exit partC
       ll = len_trim(one_line)
       ! '99' -> surface data
       if (i == 1 .and. checkdelimiter(line)) then
          long_line = one_line(1:ll-1)
          exit partC
       else if (i == 1 .and. .not.checkdelimiter(line)) then
          long_line = one_line(1:ll)
       else if (checkdelimiter(line)) then
          long_line = trim(long_line)//' '//one_line(1:ll-1)
          exit partC
       else
          long_line = trim(long_line)//' '//one_line(1:ll)
       end if
    end do partC
   
    llall = len_trim(long_line)
    np    = llall/18
    nrest = mod(llall,18)
    
    if (debug) then
      write(*,'(a)') trim(long_line)
      write(*,*) 'C1: llall = ',llall,', np = ',np, &
                 ', nrest = ',nrest,' datestring:',trim(datestring)
    end if

310 FORMAT(a2,10x,a3,a2)
311 FORMAT(a2)
312 FORMAT('Part C: '6x,f5.1' hPa',1x,i3'°',1x,i2' kt ',a)

    ! now scan the long line and look for the pressure levels

    do ip = 1, np
       ll = len_trim(long_line)
       if (ll >= 18) then
          read(long_line,310) tmp_pp(ip), tmp_dd(ip), tmp_ff(ip)
          ! shift by 19, to reach the next pressure level
          long_line = long_line(19:)
          if (tmp_pp(ip) /= '//'  .and. &
              tmp_pp(ip) /= '88'  .and. &
              tmp_pp(ip) /= '77'  .and. &
              tmp_dd(ip) /= '///' .and. &
              tmp_ff(ip) /= '//') then
             read(tmp_pp(ip),'(i3)') pp
             read(tmp_dd(ip),'(i3)') dd
             read(tmp_ff(ip),'(i2)') ff
             
             if (dd <= 360) then
               idx = idx + 1
               pp_out(idx) = pp
               dd_out(idx) = dd
               ff_out(idx) = ff
               ttc = .true.
               write(*,312) pp_out(idx), dd_out(idx), ff_out(idx), trim(datestring)
             end if
          end if
       else
          read(long_line,311) tmp_pp(ip)
          if (debug) print *,'Part C: long_line = ',trim(long_line)
          if (tmp_pp(ip) == '88' .or. tmp_pp(ip) == '77') then
             ttc = .true.
          end if
       end if
    end do
    return
  end subroutine readin_part_c
  
  subroutine readin_part_d(nin, idx, dd_out, ff_out, pp_out)

    ! Decode the Part D data. 
    ! Part D contains data on significant levels for pressures less than 100 hPa.
    
    implicit none

    integer,               intent(in)     :: nin
    integer,               intent(in out) :: idx
    integer, dimension(:), intent(in out) :: dd_out, ff_out
    real,    dimension(:), intent(in out) :: pp_out
    
    integer             :: ll, llall, ierr
    integer             :: np, ip, nrest
    integer             :: dd, ff, pp
    character(len=100)  :: one_line = ''
    character(len=1024) :: long_line = ''
    character(len=3), dimension(100) :: tmp_pp, tmp_dd, tmp_ff
    integer,          dimension(100) :: counter

    ! concatenate all lines of part D from line starting with '21212' onward
    
    long_line = ''
    partD: do
       read(nin,'(a)', iostat=ierr) one_line
       if (ierr /= 0) exit partD
       ll = len_trim(one_line)
       if (one_line(1:5) == '21212' .and. checkdelimiter(line)) then
          long_line = one_line(7:ll-1)
          exit partD
       else if (one_line(1:5) == '21212' .and. .not.checkdelimiter(line)) then
          long_line = one_line(7:ll)
       else if (checkdelimiter(line)) then
          long_line = trim(long_line)//' '//one_line(1:ll-1)
          exit partD
       else
          long_line = trim(long_line)//' '//one_line(1:ll)
       end if
    end do partD

120 FORMAT(i2,a3,1x,a3,a2)
121 FORMAT('Part D: 'i2,4x,f5.1' hPa',1x,i3'°',1x,i2' kt ',a)
122 FORMAT('Part D: 'i2,1x,i2,1x,f5.1' hPa',1x,i3'°',1x,i2' kt ',a)

    llall = len_trim(long_line)
    np    = llall/12
    nrest = mod(llall,12)
    
    if (debug) then
       write(*,'(a)') trim(long_line)
       write(*,*) 'D1: llall = ',llall,', np = ',np, &
                  ', nrest = ',nrest,' datestring:',trim(datestring)
    end  if
    do ip = 1, np
       read(long_line,120) counter(ip), tmp_pp(ip), tmp_dd(ip), tmp_ff(ip)
       ! shift by 13 
       long_line = long_line(13:)
       if (tmp_pp(ip) /= '///' .and. &
           tmp_dd(ip) /= '///' .and. &
           tmp_ff(ip) /= '//') then
          read(tmp_pp(ip),'(i3)') pp
          read(tmp_dd(ip),'(i3)') dd
          read(tmp_ff(ip),'(i2)') ff
          if ( ip == 1 .OR. &
              (ip > 1  .AND. (counter(ip-1)+11==counter(ip))) .OR. &
              (ip > 1  .AND. (counter(ip-1)==99.AND.counter(ip)==11)) ) then
             ! check for reasonable wind direction:
             if (dd <= 360) then
                idx = idx + 1
                pp_out(idx)=pp/10.0
                dd_out(idx)=dd
                ff_out(idx)=ff
                if (ip == 1) then
                   write(*,121) counter(ip), pp_out(idx), dd_out(idx), ff_out(idx), trim(datestring)
                else
                   write(*,122) counter(ip-1), counter(ip), pp_out(idx), dd_out(idx), ff_out(idx), trim(datestring)
                end if
             end if
          end if
       end if
    end do
    return
  end subroutine



end program parse_wind
