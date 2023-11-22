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
     stop
  endif

  read(*,fmt='(a)',iostat=err) windfile
  if(err/=0)then
     write(*,*) 'Fehler beim Einlesen der Outputdateibezeichnung'
     stop
  endif

  read(*,fmt='(a)',iostat=err) errfile
  if(err/=0)then
     write(*,*) 'Fehler beim Einlesen der Errordateibezeichnung'
     stop
  endif


  !Files oeffnen ------------------------------------------------
  open(nin,file=trim(infile),iostat=err)
  if(err/=0)then
     write(*,*) 'Fehler beim Oeffnen der Inputdatei'
     stop
  endif

  open(nout,file=trim(windfile),iostat=err)
  if(err/=0)then
     write(*,*) 'Fehler beim Oeffnen der Outputdatei'
     stop
  endif

  open(nerr,file=trim(errfile),iostat=err,position='append')
  if(err/=0)then
     write(*,*) 'Fehler beim Oeffnen der Outputdatei'
     stop
  endif
  !---------------------------------------------------------------

  !Flaechenzaehler 0 setzen und Datenfelder initialisieren
  idx=0
  p=-999
  d=-999
  f=-999

  counter=0
  fdcount=0

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
  read(nin,'(a54)',iostat=err) line
  if(err/=0)then
     write(nerr,*) 'D'//trim(datestring)
     write(nerr,*) 'A-Teil: Keine Daten und kein File-Delimiter'
     !write(*,*) 'A-Teil: Keine Daten und kein File-Delimiter'
     stop
  endif
  !write(*,*)len_trim(line),trim(line)
  if((line(1:4)=='TTAA').and.checkdelimiter(line))then
  elseif(len_trim(line)==1.and.checkdelimiter(line))then
     fdcount=1
  elseif(line(1:4)=='TTAA')then
     !write(*,*) 'A-Teil'
     do while(.NOT.tta)
	read(nin,'(a54)') line
	lln=len_trim(line)
 !write (*,'(i2,a54)')lln,adjustl(trim(line))
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
           read(nin,'(a54)') line
           if((line(1:2)=='88').or.(line(1:2)=='77')) then
              tta=.true.
           else
              write(nerr,*) 'D'//trim(datestring)
              write(nerr,*) 'A-Teil: Format fehlerhaft'
              !write(*,*) 'A-Teil: Format fehlerhaft'
              stop	
           endif
	endif
     enddo
  else
     write(nerr,*) 'D'//trim(datestring)
     write(nerr,*) 'A-Teil: Keine Daten und kein File-Delimiter'
     !write(*,*) 'A-Teil: Keine Daten und kein File-Delimiter'
     stop
  endif
  !-----------------------------------------------------------------

  !zum C-Teil vorspulen---------------------------------------------
  !read(nin,'(a54)') line
  do while(line(1:4)/='TTCC'.and.(.not.checkdelimiter(line)))
     read(nin,'(a54)') line
     !write (*,*) len_trim(line),trim(line)
     !write (*,'(a54)') trim(line)
  enddo
  if(line(1:4)=='TTCC')then
     backspace(nin)
  elseif(checkdelimiter(line)) then
     fdcount=1
  endif
  !-----------------------------------------------------------------

  !C Teil Ueberpruefen----------------------------------------------
  ttc=.false.
  read(nin,'(a54)',iostat=err) line
  if(err/=0)then
     write(nerr,*) 'D'//trim(datestring)
     write(nerr,*) 'C-Teil: Keine Daten und kein File-Delimiter'
     !write(*,*) 'C-Teil: Keine Daten und kein File-Delimiter'
     stop
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
     do while(.NOT.ttc)
	read(nin,'(a54)') line
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
           read (nin,'(a54)') line
           lln = len_trim(line)
           if (line(1:2)=='77') then
              ttc=.true.
           else
              write(nerr,*) 'D'//trim(datestring)
              write(nerr,*) 'C-Teil: Format fehlerhaft'
              !write(*,*) 'C-Teil: Format fehlerhaft'
              stop
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
           read (nin,'(a54)') line2
           lln2 = len_trim(line2)
           !write (*,'(i2,a54)')lln2,adjustl(trim(line2))
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
              stop	
           endif
	elseif (lln==1) then
           ttc = .true.
	else
           read (nin,'(a54)') line2
           read (nin,'(a54)') line3
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
                 stop
              endif
           else
              write (nerr,*) 'D'//trim(datestring)
              write (nerr,*) 'C-Teil: Format fehlerhaft'
              !write(*,*) 'C-Teil: Format fehlerhaft'
              stop	
           endif
	endif
     enddo
  else	
     write (nerr,*) 'D'//trim(datestring)
     write (nerr,*) 'C-Teil: Keine Daten und kein File-Delimiter'
     !write(*,*) 'C-Teil: Keine Daten und kein File-Delimiter'
     stop
  endif
  !-----------------------------------------------------------------


  !zum D-Teil vorspulen---------------------------------------------
  !write (*,'(a54)') trim(line)
  do while (line(1:4) /= 'TTDD' .and. &
       (.not.checkdelimiter(line)))
     read (nin,'(a54)') line
     !write (*,*) len_trim(line),trim(line)
     !write (*,'(a54)') trim(line)
  enddo
  if (line(1:4)=='TTDD') then
     backspace(nin)
  endif
  !--------------------------------------------------------------------------------------


  !D Teil Ueberpruefen---------------------------------------------------------------------
  ttd = .false.
  read (nin,'(a54)',iostat=err) line
  if (err/=0) then
     write (nerr,*) 'D'//trim(datestring)
     write (nerr,*) 'D-Teil: Keine Daten und kein File-Delimiter'
     !write(*,*) 'D-Teil: Keine Daten und kein File-Delimiter'
     stop
  endif
  !write (*,*) trim(line)
  if ((line(1:4)=='TTDD') .and. &
       (checkdelimiter(line))) then
  elseif (checkdelimiter(line)) then
  elseif (line(1:4)=='TTDD') then
     !write(*,*) 'D-Teil'
     do while(line(1:5)/='21212')
        read(nin,'(a54)') line
        if(checkdelimiter(line)) stop
     enddo
     backspace(nin)
     ic=0
     read(nin,'(a54)') line
     lln=len_trim(line)
     !write (*,'(i2,a54)')lln,adjustl(trim(line))
     line=line(7:)
     lln=len_trim(line)
     if(mod(lln,12)==0.and.checkdelimiter(line))then
        c=lln/12
        !write(*,*) 'delimiter und format stimmt',c
        do i=1,c
           read(line,'(i2,a3,1x,a3,a2)')counter(i),tmp_pp(i),tmp_dd(i),tmp_ff(i)
           line=line(13:)
           if(tmp_pp(i)/='///'.and.tmp_dd(i)/='///'.and.tmp_ff(i)/='//')then
              read(tmp_pp(i),'(i3)') pp
              read(tmp_dd(i),'(i3)') dd
              read(tmp_ff(i),'(i3)') ff
              if(i>1.and.(counter(i-1)==counter(i)+11))then
                 idx=idx+1
                 p(idx)=pp/10.0
                 d(idx)=dd
                 f(idx)=ff
              else
                 write(nerr,*) 'D'//trim(datestring)
                 write(nerr,*) 'D-Teil: Nummerierung fehlerhaft'
                 !write(*,*) 'D-Teil: Nummerierung fehlerhaft'
                 stop
              endif
           endif
        end do
        !write(fmt,'(4a,i1,22a)') '(a6,',c,'(f4.1,1x,i3,1x,i2,3x))'
        !write(*,trim(fmt)) 'Daten:',(p(i),d(i),f(i),i=idx+1-c,idx)
     elseif(lln==47.and.(.not.checkdelimiter(line)))then
        c=4
        !ersten 4 Spalten lesen
        do i=1,c
           ic=ic+1
           read(line,'(i2,a3,1x,a3,a2)')counter(ic),tmp_pp(i),tmp_dd(i),tmp_ff(i)
           line=line(13:)
           !write(*,'(a3,2x,a54)') 'neu',line
           if(tmp_pp(i)/='///'.and.tmp_dd(i)/='///'.and.tmp_ff(i)/='//')then
              read(tmp_pp(i),'(i3)') pp
              read(tmp_dd(i),'(i3)') dd
              read(tmp_ff(i),'(i3)') ff
              if(ic==1)then
                 idx=idx+1
                 p(idx)=pp/10.0
                 d(idx)=dd
                 f(idx)=ff
                 !write(*,*)p(idx),pp
                 !write(*,*)d(idx),dd
                 !write(*,*)f(idx),ff
              elseif(ic>1.and.(counter(ic-1)+11==(counter(ic))))then
                 idx=idx+1
                 p(idx)=pp/10.0
                 d(idx)=dd
                 f(idx)=ff
                 !write(*,*)p(idx),pp
                 !write(*,*)d(idx),dd
                 !write(*,*)f(idx),ff

              else
                 write(nerr,*) 'D'//trim(datestring)
                 write(nerr,*) 'D-Teil: Nummerierung fehlerhaft'
                 !write(*,*) 'D-Teil: Nummerierung fehlerhaft'
                 stop
              endif
           endif
        end do
        !rest einlesen
        do while(.not.ttd)
           !write(*,*) 'mehrere Zeilen lesen'
           read(nin,'(a54)') line
           lln=len_trim(line)
           !write (*,'(i2,a54)')lln,adjustl(trim(line))
           if(mod(lln,12)==0.and.checkdelimiter(line))then
              c=lln/12
              do i=1,c
                 ic=ic+1
                 read(line,'(i2,a3,1x,a3,a2)')counter(ic),tmp_pp(i),tmp_dd(i),tmp_ff(i)
                 line=line(13:)
                 if(tmp_pp(i)/='///'.and.tmp_dd(i)/='///'.and.tmp_ff(i)/='//')then
                    read(tmp_pp(i),'(i3)') pp
                    read(tmp_dd(i),'(i3)') dd
                    read(tmp_ff(i),'(i3)') ff
                    if(ic>1.and.((counter(ic-1)+11==counter(ic)).or.(counter(ic-1)==99.and.counter(ic)==11)))then
                       idx=idx+1
                       p(idx)=pp/10.0
                       d(idx)=dd
                       f(idx)=ff
                    else
                       write(nerr,*) 'D'//trim(datestring)
                       write(nerr,*) 'D-Teil: Nummerierung fehlerhaft'
                       !write(*,*) 'D-Teil: Nummerierung fehlerhaft'
                       stop
                    endif
                 endif
              enddo
              ttd=.true.
           elseif(lln==53.and.(.not.checkdelimiter(line)))then
              read(nin,'(a54)') line2
              !write (*,'(i2,a54)')lln,adjustl(trim(line2))
              c=5
              do i=1,c
                 ic=ic+1
                 if(i==5)then
                    read(line,'(i2,a3)') counter(ic),tmp_pp(i)
                    !write(*,'(i2,a3)') counter(ic),tmp_pp(i)
                 else
                    read(line,'(i2,a3,1x,a3,a2)')counter(ic),tmp_pp(i),tmp_dd(i),tmp_ff(i)
                    line=line(13:)
                    !write(*,'(a3,2x,a54)') 'neu',line
                    if(tmp_pp(i)/='///'.and.tmp_dd(i)/='///'.and.tmp_ff(i)/='//')then
                       read(tmp_pp(i),'(i3)') pp
                       read(tmp_dd(i),'(i3)') dd
                       read(tmp_ff(i),'(i3)') ff
                       if(ic>1.and.((counter(ic-1)+11==counter(ic)).or.(counter(ic-1)==99.and.counter(ic)==11)))then
                          idx=idx+1
                          p(idx)=pp/10.0
                          d(idx)=dd
                          f(idx)=ff
                       else
                          write(nerr,*) 'D'//trim(datestring)
                          write(nerr,*) 'D-Teil: Nummerierung fehlerhaft'
                          !write(*,*) 'D-Teil: Nummerierung fehlerhaft'
                          stop
                       endif
                    endif
                 endif
              enddo
              i=5
              !write(*,*) 'read line one done'
              read(line2,'(a3,a2)',iostat=err) tmp_dd(i),tmp_ff(i)
              if(err/=0)then
                 !write(nerr,*) 'D'//trim(datestring)
                 !write(nerr,*) 'D-Teil: Format fehlerhaft'
                 stop  
              endif
              !write(*,'(i2,a3,1x,a3,a2)')counter(ic),tmp_pp(i),tmp_dd(i),tmp_ff(i)
              if(tmp_pp(i)/='///'.and.tmp_dd(i)/='///'.and.tmp_ff(i)/='//')then
                 read(tmp_pp(i),'(i3)') pp
                 read(tmp_dd(i),'(i3)') dd
                 read(tmp_ff(i),'(i3)') ff
                 if(ic>1.and.((counter(ic-1)+11==counter(ic)).or.(counter(ic-1)==99.and.counter(ic)==11)))then
                    idx=idx+1
                    p(idx)=pp/10.0
                    d(idx)=dd
                    f(idx)=ff
                 else
                    write(nerr,*) 'D'//trim(datestring)
                    write(nerr,*) 'D-Teil: Nummerierung fehlerhaft'
                    !write(*,*) 'D-Teil: Nummerierung fehlerhaft'
                    stop
                 endif
              endif

              !write(*,*) 'read rest od second line'
              line2=line2(7:)
              lln2=len_trim(line2)
              !write (*,'(i2,a54)')lln2,adjustl(trim(line2))
              !write(*,*) ic
              if(mod(lln2,12)==0.and.checkdelimiter(line2))then
                 c=(lln2)/12
                 do i=1,c
                    ic=ic+1
                    read(line2,'(i2,a3,1x,a3,a2)')counter(ic),tmp_pp(i),tmp_dd(i),tmp_ff(i)
                    !write(*,'(i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,a3,1x,a3,a2)')i,ic,c,counter(ic-1),counter(ic),tmp_pp(i),tmp_dd(i),tmp_ff(i)
                    line2=line2(13:)
                    if(tmp_pp(i)/='///'.and.tmp_dd(i)/='///'.and.tmp_ff(i)/='//')then
                       read(tmp_pp(i),'(i3)') pp
                       read(tmp_dd(i),'(i3)') dd
                       read(tmp_ff(i),'(i3)') ff
                       if(ic>1.and.((counter(ic-1)+11==counter(ic)).or.(counter(ic-1)==99.and.counter(ic)==11)))then
                          idx=idx+1
                          p(idx)=pp/10.0
                          d(idx)=dd
                          f(idx)=ff
                       else
                          write(nerr,*) 'D'//trim(datestring)
                          write(nerr,*) 'D-Teil: Nummerierung fehlerhaft'
                          !write(*,*) 'D-Teil: Nummerierung fehlerhaft'
                          stop
                       endif
                    endif
                 enddo
                 ttd=.true.
              elseif(lln2==47.and.(.not.checkdelimiter(line2)))then
                 c=4
                 do i=1,c
                    ic=ic+1
                    read(line2,'(i2,a3,1x,a3,a2)')counter(ic),tmp_pp(i),tmp_dd(i),tmp_ff(i)
                    line2=line2(13:)
                    !write(*,'(a3,2x,a54)') 'neu',line2
                    if(tmp_pp(i)/='///'.and.tmp_dd(i)/='///'.and.tmp_ff(i)/='//')then
                       read(tmp_pp(i),'(i3)') pp
                       read(tmp_dd(i),'(i3)') dd
                       read(tmp_ff(i),'(i3)') ff
                       if(ic>1.and.((counter(ic-1)+11==counter(ic)).or.(counter(ic-1)==99.and.counter(ic)==11)))then
                          idx=idx+1
                          p(idx)=pp/10.0
                          d(idx)=dd
                          f(idx)=ff
                          !write(*,*)p(idx),pp
                          !write(*,*)d(idx),dd
                          !write(*,*)f(idx),ff
                       else
                          write(nerr,*) 'D'//trim(datestring)
                          write(nerr,*) 'D-Teil: Nummerierung fehlerhaft'
                          !write(*,*) 'D-Teil: Nummerierung fehlerhaft'
                          stop
                       endif
                    endif
                 end do
              elseif(lln2==0)then
                 ttd=.true.
              else
                 write(nerr,*) 'D'//trim(datestring)
                 write(nerr,*) 'D-Teil: Format fehlerhaft'
                 !write(*,*) 'D-Teil: Format fehlerhaft'
                 stop  
              endif
           endif
        end do
     else
        write(nerr,*) 'D'//trim(datestring)
        write(nerr,*) 'D-Teil: Format fehlerhaft'
        !write(*,*) 'D-Teil: Format fehlerhaft'
        stop
     endif
  else	
     write(nerr,*) 'D'//trim(datestring)
     write(nerr,*) 'D-Teil: Keine Daten und kein File-Delimiter'
     !write(*,*) 'D-Teil: Keine Daten und kein File-Delimiter'
     stop
  endif
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



end program parse_wind
