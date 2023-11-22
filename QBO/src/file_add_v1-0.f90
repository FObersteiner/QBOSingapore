
program file_add

  implicit none

  integer               :: l_sing_path,anz_arg

  integer,dimension(21) :: wind_u
  integer               :: i,date,year

  character (len=256)   :: inputfile
  character (len=256)   :: qbo_file,   qbo_cache
  character (len=256)   :: sing_file,  sing_path 
  character (len=256)   :: singa_file, singa_cache
  character (len=125)   :: argv

  common /param/ date, year, wind_u,qbo_file, singa_file

  anz_arg = iargc()
  !!print*,anz_arg
  if(anz_arg.ne.1) then
     print*,'add_file.f90 ERROR'
     print*,'add_file sing_path'
     print*,'anzahl der parameter falsch'
     stop
  endif


  call getarg(1, argv)
  read(argv, '(125a)') sing_path

  inputfile=trim(sing_path)//'/'//'tmp.dat'

  l_sing_path=len_trim(sing_path)

  qbo_file=sing_path(:l_sing_path-9)//'/'//'qbo.dat'
  qbo_cache=sing_path(:l_sing_path-9)//'/'//'qbo_cache.dat'
  singa_file=sing_path(:l_sing_path-9)//'/'//'singapore.dat'
  singa_cache=sing_path(:l_sing_path-9)//'/'//'singapore_cache.dat'

  open(31,file=inputfile)

  read(31,'(I4,7X,I4,21(1X,I4))') year,date,(wind_u(i), i=1,21)

  write(*,*) 'Jahr:',year,'Date:',date
  write(*,*) 'Filepath: '
  write(*,*) trim(qbo_file)

  !do i=1,21
  !	write (*,*) 'Winspseed in U:',wind_u(i)
  !end do

  call add_qbo(date,wind_u,qbo_file,qbo_cache)
  call add_singapore(date,year,wind_u,singa_file,singa_cache)

contains

  subroutine add_qbo(date,wind_u,qbo_file,qbo_cache)

    integer               ::  date,dateidx
    integer,dimension(21) ::  wind_u

    integer               ::  error

    character (len=256)   ::  line,qbo_file,qbo_cache
    character (len=5)     ::  statidx

    logical               ::  hit = .false.
    logical               ::  eof = .false.

    !write(*,*) trim(qbo_file)

    open(32,file=qbo_file)
    open(33,file=qbo_cache)


    do while(.not. eof)
       read(32,'(a256)',iostat=error) line
       if(error==-1) then
          eof=.true.
          if(.not.hit) then
             write(33,'(I5,1X,I4.4,2X,I4,6(3X,I4))') &
                  48698, date,                       &
                  wind_u(4),  wind_u(6),  wind_u(8), wind_u(10), &
                  wind_u(13), wind_u(14), wind_u(16)
          endif
       else
          !write (*,*) trim(line)
          !line=line(2:)
          read(line,'(A5)') statidx
          if((.not.hit).and.statidx=='48698') then
             read(line,'(6X,I4.4)')dateidx
             if(dateidx==date) then
                !zeile gefunden,backtrack,zeile neu schreiben
                !write(*,*) 'Zeile in qbo.dat gefunden'
                hit=.true.
                write(33,'(I5,1X,I4.4,2X,I4,6(3X,I4))')  &
                     48698, date,                        &
                     wind_u(4),  wind_u(6),  wind_u(8), wind_u(10), &
                     wind_u(13), wind_u(14), wind_u(16)
                !write(33,'(I5,1X,I4.4,2X,I4,6(3X,I4))') &
                !      48698, date, 9999,                &
                !      wind_u(6), wind_u(8), wind_u(10), &
                !      wind_u(13),wind_u(14),-888
                cycle
             endif
          endif
          !write(33,'(A)',advance='no') '',trim(line)
          write(33,'(A)') trim(line)
       endif
    end do

    close(32)
    close(33)



  end subroutine add_qbo

  subroutine add_singapore(date,year,wind_u,singa_file,singa_cache)

    integer                  :: year,yearidx,i,j,date,month
    integer,dimension(21)    :: wind_u
    integer,dimension(15,13) :: qbo_data
    integer,dimension(13,15) :: qbo_trans
    integer,dimension(15)    :: qbo_line
    integer,dimension(15)    :: level = (/ 10, 12, 15, 20, 25, &
                                           30, 35, 40, 45, 50, &
                                           60, 70, 80, 90, 100 /)

    integer            :: error
    character(len=256) :: line,singa_file,singa_cache
    logical            :: hit=.false.
    logical            :: eof=.false.

    open(32,file=singa_file)
    open(33,file=singa_cache)

    month=mod(date,100)
    !read(date,'(2X,I2)') month
    !write(*,*) 'month:',month


    do while(.not. eof)
       read(32,'(a256)',iostat=error) line
       if(error==-1) then
          eof=.true.
          if(.not.hit) then
             !zeile gefunden,backtrack,zeile neu schreiben
             !write (*,*) 'yearidx:',yearidx		
             !write(*,*) 'Zeile nicht in singapore.dat gefunden und anhaengen'
             write(33,'(/,I4)') year
             write(33,'(A3,12(2X,A3))') 'hPa',         &
                  'JAN','FEB','MAR','APR','MAY','JUN', &
                  'JUL','AUG','SEP','OCT','NOV','DEC'
             do i=1,15
                if(i<=4) then
                   write(33,'(I3,1X,I4)') level(i),wind_u(17-i)
                else
                   write(33,'(I3,1X,I4)') level(i),wind_u(16-i)
                end if
             end do
             !write(33,'(I5,1X,I4.4,2X,I4,6(3X,I4))') 48698,date, &
             !          wind_u(4),wind_u(6),wind_u(8),wind_u(10), &
             !          wind_u(13),wind_u(14),wind_u(16)
          endif
       else
          !write (*,*) trim(line)
          !line=line(2:)
          read(line,'(I4)',iostat=error) yearidx
          if((error==0).and.(yearidx==year)) then
             !zeile gefunden,backtrack,zeile neu schreiben
             !write (*,*) 'yearidx:',yearidx
             !write(*,*) 'Zeile in singapore.dat gefunden'
             hit=.true.
             write(33,'(I4)') year
             read(32,'(1x)')				
             write(33,'(A3,12(2X,A3))') 'hPa',          &
                  'JAN','FEB','MAR','APR','MAY','JUN', &
                  'JUL','AUG','SEP','OCT','NOV','DEC'
             do i=1,15
                read(32,'(I3,12(1X,I4))') (qbo_data(i,j),j=1,13)
             end do
             qbo_trans=transpose(qbo_data)
             do i=2,13
                qbo_line=qbo_trans(i,:)
                if(all(qbo_line==0,1)) then
                   qbo_trans(i,:)=-999
                endif
             end do
             qbo_data=transpose(qbo_trans)
             do i=1,15	
                if(i<=4) then
                   qbo_data(i,month+1)=wind_u(17-i)
                else
                   qbo_data(i,month+1)=wind_u(16-i)
                end if
                write(33,'(I3,12(1X,I4))') (qbo_data(i,j),j=1,13)
             end do
             cycle
          endif
          !write(33,'(A)',advance='no') '',trim(line)
          write(33,'(A)') trim(line)
       endif

    end do

    close(32)
    close(33)

  end subroutine add_singapore

end program file_add
