program parse_wind

  !Fortran 90 Portierung von singa2.f von Naujokat   
  !Stefan Metzner FU Berlin 2012

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
  if((line(1:4)=='TTAA').and.checkdelimiter(line))then
  elseif(len_trim(line)==1.and.checkdelimiter(line))then
     fdcount=1
  elseif(line(1:4)=='TTAA')then
     !write(*,*) 'A-Teil'
     do while(tta .EQV. .false.)
	read(nin,71) line
	lln=len_trim(line)
    write (*,'(i2,1x,a,1x,a,1x,l1)')lln,'A1',adjustl(trim(line)),checkdelimiter(line)
	if(lln==53.or.(lln==54.and.checkdelimiter(line)))then
           read(line,'(36x,a2,10x,a3,a2)')tmp_pp(1),tmp_dd(1),tmp_ff(1)
           if(tmp_pp(1)/='//'.and.tmp_dd(1)/='///'.and.tmp_ff(1)/='//')then
              read(tmp_pp(1),'(i3)')pp
              read(tmp_dd(1),'(i3)')dd
              read(tmp_ff(1),'(i2)')ff
              !write(*,*) pp,dd,ff
              if(pp==10)then
                 !write(*,'(a6,1x,f5.1,1x,i3.3,1x,i3)') 'Daten:',pp*10.0,dd,ff
                 idx=1
                 p(idx)=pp*10.0
                 d(idx)=dd
                 f(idx)=ff
                 tta=.true.
              end if
           else
              tta=.true.
           endif
           fdcount=1
	elseif(checkdelimiter(line))then
           tta=.true.
           fdcount=1
	else
           read(nin,71) line
           if((line(1:2)=='88').or.(line(1:2)=='77')) then
              tta=.true.
           else
              write(nerr,*) 'D'//trim(datestring)
              write(nerr,*) 'A-Teil: Format fehlerhaft'
              !write(*,*) 'A-Teil: Format fehlerhaft'
              call exit(1)
           endif
	endif
     enddo
  else
     write(nerr,*) 'D'//trim(datestring)
     write(nerr,*) 'A-Teil: Keine Daten und kein File-Delimiter'
     !write(*,*) 'A-Teil: Keine Daten und kein File-Delimiter'
     call exit(err)
  endif
  !-----------------------------------------------------------------

  !zum C-Teil vorspulen---------------------------------------------
  !read(nin,71) line
  do while(line(1:4)/='TTCC'.and.(.not.checkdelimiter(line)))
     read(nin,71) line
     !write (*,*) len_trim(line),trim(line)
     !write (*,71) trim(line)
  enddo
  if(line(1:4)=='TTCC')then
     backspace(nin)
  elseif(checkdelimiter(line)) then
     fdcount=1
  endif
  !-----------------------------------------------------------------

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
  if((line(1:4)=='TTCC').and.(checkdelimiter(line)))then
     !write(*,*) 'D'//trim(datestring)
     !write(*,*) 'TTCC+delimiter'
     fdcount=fdcount+1
  elseif(len_trim(line)==1.and.checkdelimiter(line))then
     fdcount=fdcount+1
  elseif(line(1:4)=='TTCC')then
     !write(*,*) 'C-Teil'
     do while(ttc .EQV. .false.)
	read(nin,71) line
	lln=len_trim(line)

 !write (*,'(i2,a54)')lln,adjustl(trim(line))
	if((mod(lln,18)==0).and.checkdelimiter(line))then
    !daten auslesen
           c=lln/18
           read(line,'(3(a2,10x,a3,a2,1x))')  &
                      (tmp_pp(i), tmp_dd(i), tmp_ff(i), i=1,c)
           do i = 1, c 
              if ( tmp_pp(i)/='//'  .and. &
                   tmp_dd(i)/='///' .and. &
                   tmp_ff(i)/='//' ) then
                 read (tmp_pp(i),'(i3)') pp
                 read (tmp_dd(i),'(i3)') dd
                 read (tmp_ff(i),'(i3)') ff
                 idx = idx + 1
                 p(idx) = pp
                 d(idx) = dd
                 f(idx) = ff
              endif
           enddo
           !write(*,'(a6,3(f4.1,1x,i3.3,1x,i3,3x))')  &
           !         'Daten:',(p(i),d(i),f(i),i=idx-2,idx)
           ttc=.true.
           fdcount=fdcount+1
	elseif(lln==5.and.line(1:2)=='88')then
           read (nin,71) line
           lln = len_trim(line)
           if (line(1:2)=='77') then
              ttc=.true.
           else
              write(nerr,*) 'D'//trim(datestring)
              write(nerr,*) 'C-Teil: Format fehlerhaft'
              !write(*,*) 'C-Teil: Format fehlerhaft'
              call exit(err)
           endif
	elseif(lln==53)then
           !daten auslesen 
           read(line,'(3(a2,10x,a3,a2,1x))')  &
                      (tmp_pp(i), tmp_dd(i), tmp_ff(i), i = 1,3)
           do i = 1, 3 
              if ( tmp_pp(i)/='//' .and. &
                   tmp_dd(i)/='///'.and. &
                   tmp_ff(i)/='//' ) then
                 read (tmp_pp(i),'(i3)') pp
                 read (tmp_dd(i),'(i3)') dd
                 read (tmp_ff(i),'(i3)') ff
                 idx = idx + 1
                 !write (*,*) idx
                 p(idx) = pp
                 d(idx) = dd	
                 f(idx) = ff
                 !write(*,*)p(idx),pp
                 !write(*,*)d(idx),dd
                 !write(*,*)f(idx),ff
              endif
           enddo
           !und naechste zeile checken
           read (nin,71) line2
           lln2 = len_trim(line2)
           write (*,'(i2,1x,a,1x,a,1x,l1)')lln2,'C1',adjustl(trim(line2)),checkdelimiter(line2)
           if((line2(1:2)=='88').or.(line(1:2)=='77').or.(line(1:1)=='=')) then
              !write(*,'(a6,3(f4.1,1x,i3.3,1x,i3,3x))') 'Daten:',(p(i),d(i),f(i),i=idx-2,idx)
              ttc = .true.
           elseif (lln2==35 .or. &
                  (lln2==36 .and. checkdelimiter(line2)) ) then
              !daten aus zeile 2 verwerten
              read (line2,'(2(a2,10x,a3,a2,1x))') &
                           (tmp_pp(i), tmp_dd(i), tmp_ff(i), i=1,2)
              do i = 1, 2 
                 if ( tmp_pp(i)/='//' .and. &
                      tmp_dd(i)/='///'.and. &
                      tmp_ff(i)/='//' ) then
                    read (tmp_pp(i),'(i3)') pp
                    read (tmp_dd(i),'(i3)') dd
                    read (tmp_ff(i),'(i3)') ff
                    idx = idx + 1
                    p(idx) = pp
                    d(idx) = dd	
                    f(idx) = ff
                    !write(*,*)p(idx),pp
                    !write(*,*)d(idx),dd
                    !write(*,*)f(idx),ff
                 endif
              enddo
              !write(*,*)'idx',idx
              !write(*,'(a6,3(i3,1x,i3.3,1x,i3,3x))')   &
              !         'Daten:',(p(i),d(i),i,i=idx-4,idx-2)
              !write(*,'(a6,3(f4.1,1x,i3.3,1x,i3,3x))') &
              !         'Daten:',(p(i),d(i),f(i),i=idx-4,idx-2)
              !write(*,'(a6,2(i3,1x,i3.3,1x,i3,3x))') '      ', &
              !         (p(i),d(i),i,i=idx-1,idx)
              !write(*,'(a6,2(f4.1,1x,i3.3,1x,i3,3x))') '      ', &
              !         (p(i),d(i),f(i),i=idx-1,idx)
              ttc = .true.
           elseif (lln2==17 .or. &
                  (lln2==18 .and. checkdelimiter(line2)) ) then
              !daten aus zeile 2 verwerten
              read (line2,'(1(a2,10x,a3,a2,1x))')  &
                           (tmp_pp(i), tmp_dd(i), tmp_ff(i), i=1,1)
              do i = 1, 1
                 if ( tmp_pp(i)/='//'  .and.  &
                      tmp_dd(i)/='///' .and.  &
                      tmp_ff(i)/='//' ) then
                    read (tmp_pp(i),'(i3)') pp
                    read (tmp_dd(i),'(i3)') dd
                    read (tmp_ff(i),'(i3)') ff
                    idx = idx + 1
                    p(idx) = pp
                    d(idx) = dd	
                    f(idx) = ff
                 endif
              enddo
              !write(*,'(a6,3(f4.1,1x,i3.3,1x,i3,3x))') &
              !         'Daten:',(p(i),d(i),f(i),i=idx-3,idx-1)
              !write(*,'(a6,2(f4.1,1x,i3.3,1x,i3,3x))') '      ', &
              !          (p(i),d(i),f(i),i=idx,idx)
              ttc = .true.
           elseif (lln2<17 .or.  &
                  (lln2<18 .and. checkdelimiter(line2)) ) then
              ttc = .true.
           else
              write (nerr,*) 'D'//trim(datestring)
              write (nerr,*) 'C-Teil: Format fehlerhaft'
              !write(*,*) 'C-Teil: Format fehlerhaft'
              call exit(1)
           endif
	elseif (lln==1) then
           ttc = .true.
	else
           read (nin,71) line2
           read (nin,71) line3
           lln2 = len_trim(line2)
           lln3 = len_trim(line3)
           !write (*,'(i2,a54)')lln2,adjustl(trim(line2))
           !write (*,'(i2,a54)')lln3,adjustl(trim(line3))
           if (((line2(1:2)=='88') .and. (line3(1:2)=='77')) .or. &
                (line2(1:2)=='77'  .and. checkdelimiter(line2))) then
              ttc = .true.
              !daten aus Zeile 1 auswerten
              if(lln==35)then
                 read (line,'(2(a2,10x,a3,a2,1x))') &
                             (tmp_pp(i), tmp_dd(i), tmp_ff(i), i=1,2)
                 do i=1,2 
                    if ( tmp_pp(i) /= '//'  .and. &
                         tmp_dd(i) /= '///' .and. &
                         tmp_ff(i) /= '//') then
                       read (tmp_pp(i),'(i3)') pp
                       read (tmp_dd(i),'(i3)') dd
                       read (tmp_ff(i),'(i3)') ff
                       idx = idx+1
                       p(idx) = pp
                       d(idx) = dd	
                       f(idx) = ff
                    endif
                 enddo
                 !write(*,'(a6,2(f4.1,1x,i3.3,1x,i3,3x))') &
                 !         'Daten:',(p(i),d(i),f(i),i=idx-1,idx)
              elseif(lln==17)then
                 read (line,'(1(a2,10x,a3,a2,1x))') &
                             (tmp_pp(i), tmp_dd(i), tmp_ff(i), i=1,1)
                 do i=1,1 
                    if (tmp_pp(i) /= '//' .and. &
                        tmp_dd(i) /= '///'.and. &
                        tmp_ff(i) /= '//') then
                       read (tmp_pp(i),'(i3)') pp
                       read (tmp_dd(i),'(i3)') dd
                       read (tmp_ff(i),'(i3)') ff
                       idx = idx + 1
                       p(idx) = pp
                       d(idx) = dd	
                       f(idx) = ff
                    endif
                 enddo
                 !write(*,'(a6,2(f4.1,1x,i3.3,1x,i3,3x))') &
                 !         'Daten:',(p(i),d(i),f(i),i=idx,idx)
              else
                 write (nerr,*) 'D'//trim(datestring)
                 write (nerr,*) 'C-Teil: Format fehlerhaft'
                 !write(*,*) 'C-Teil: Format fehlerhaft'
                 call exit(err)
              endif
           else
              write (nerr,*) 'D'//trim(datestring)
              write (nerr,*) 'C-Teil: Format fehlerhaft'
              !write(*,*) 'C-Teil: Format fehlerhaft'
              call exit(1)
           endif
	endif
     enddo
  else	
     write (nerr,*) 'D'//trim(datestring)
     write (nerr,*) 'C-Teil: Keine Daten und kein File-Delimiter'
     !write(*,*) 'C-Teil: Keine Daten und kein File-Delimiter'
     call exit(err)
  endif
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
  
  subroutine readin_part_d(nin, idx, dd_out, ff_out, pp_out)

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
121 FORMAT(i2,1x,a3,1x,a3,1x,a2)
122 FORMAT(i2,1x,i2,1x,'pp='a3,1x,'dd='a3,1x,'ff='a2)

    llall = len_trim(long_line)
    np    = llall/12
    nrest = mod(llall,12)
    write(*,'(a)') trim(long_line)
    write(*,*) 'D1: llall = ',llall,', np = ',np, &
               ', nrest = ',nrest,' datestring:',trim(datestring)
    do ip = 1, np
       read(long_line,120) counter(ip), tmp_pp(ip), tmp_dd(ip), tmp_ff(ip)
       ! shift by 13 
       long_line = long_line(13:)
       if (tmp_pp(ip) /= '///' .and. &
           tmp_dd(ip) /= '///' .and. &
           tmp_ff(ip) /= '//') then
          read(tmp_pp(ip),'(i3)') pp
          read(tmp_dd(ip),'(i3)') dd
          read(tmp_ff(ip),'(i3)') ff
          if ( ip == 1 .OR. &
              (ip > 1  .AND. (counter(ip-1)+11==counter(ip))) .OR. &
              (ip > 1  .AND. (counter(ip-1)==99.AND.counter(ip)==11)) ) then
             ! check for reasonable wind direction:
             if (dd <= 360) then
                if (ip == 1) then
                   write(*,121) counter(ip), tmp_pp(ip), tmp_dd(ip), tmp_ff(ip)
                else
                   write(*,122) counter(ip-1), counter(ip), tmp_pp(ip), tmp_dd(ip), tmp_ff(ip)
                end if
                idx = idx + 1
                p(idx)=pp/10.0
                d(idx)=dd
                f(idx)=ff
             end if
          end if
       end if
    end do
    return
  end subroutine



end program parse_wind
