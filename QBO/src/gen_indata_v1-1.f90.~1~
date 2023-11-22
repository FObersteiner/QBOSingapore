program gen_indata

  implicit none

  !integer,dimension(70) :: pp,ff,dd,kenn

  !character*1,dimension(70) :: aa

  character (len=1)   :: part

  character (len=125) :: obsfile
  character (len=256) :: path, sing_path, header
  character (len=256) :: outputfile, inputfile

  integer             :: anz_arg
  character (len=10)  :: date
  integer             :: yyyy
  character (len=125) :: argv

  !common /filepart/ part

  anz_arg = iargc()
  !!print*,anz_arg
  if(anz_arg.ne.4) then
     print*,'gen_indata ERROR'
     print*,'./gen_indata file path_obs date'
     print*,'anzahl der parameter falsch'
     stop
  endif

  call getarg(1, argv)
  read(argv, '(125a)') obsfile
  !WRITE(*,*) 'file: ',trim(obsfile)

  call getarg(2, argv)
  read(argv, '(125a)') path
  !WRITE(*,*) 'path: ',path

  call getarg(3, argv)
  read(argv, '(125a)') sing_path

  call getarg(4, argv)
  read(argv, '(10a)') date
  read(date, '(i4)') yyyy
  part=obsfile(6:6)
  
  !print *,'part = ',part,', obsfile = ',trim(obsfile)

  outputfile=trim(sing_path)//'/'//'sing'//date(1:8)//'.'//date(9:10)

  inputfile=trim(path)//'/'//trim(obsfile)

  !write(*,*) 'outputfile: ',trim(outputfile)
  open(31,file=inputfile,action='read')
  
  header = adjustl('sing'//date(1:8)//'.'//date(9:10))
  select case (part)
    case('a')
      open(41,file=outputfile, status='unknown')
      !rewind(41)
      write(41,'(a)') trim(header)
    case default
      open(41,file=outputfile, status='old')
      call forward
  end select
  !write(*,*) 'vorgespult'

  call copy(part)

  close(31)
  close(41)

contains

  subroutine forward

    implicit none

    character (len=125)   :: inputfile
    character (len=256)   :: line

    integer               :: error

    logical               :: eof =.false.



    do while(.not. eof)
       read(41,'(a256)',iostat=error) line
       if(error==-1) then
          eof=.true.
       else
          !write(*,*) 'error forward: ',error
       endif
    end do

  end subroutine forward


  subroutine copy(part)

    implicit none

    character (len=125)   :: inputfile
    character (len=256)   :: line, test_head
    character(len=1)      :: part

    integer,dimension(100):: pp,dd,ff

    character (len=4)           :: partidx
    integer               :: stationidx,error,length
    logical               :: sof =.false.
    logical               :: eof =.false.

    !write(*,*) 'part: ',part
    !write(*,*) eof,sof
    do while(.not. sof)
       !!!! ab 2012 schreiben die nicht mehr TTCC_59001 48698 sondern TTCC__59001 48698
       !!!! ab 2015.08 schreiben sie es machmal wieder, daher geaendert
       ! vorher:
       !if(yyyy>=2012)then
       !     read(31,'(a4,i5,i5)',iostat=error) partidx,stationidx
       !  else
       !     read(31,'(a4,7x,i5)',iostat=error) partidx,stationidx
       !  endif
       
       !write(*,*) 'read input'
       read(31,'(a4,7x,i6)',iostat=error) partidx,stationidx
       !write(*,*) partidx, stationidx, 'error=',error
       
       if(part=='x') then
          write(*,*) eof,sof
          write(*,*) partidx,'       ',stationidx
          write(*,*) 'error=',error
       endif
       if(error==-1) then
          !write(*,*) 'end of file'
          !sof=.true.
          eof=.true.
          test_head = ''
          BACKSPACE(41)
          read(41,'(a)',iostat=error) test_head
          if (error /= 0) then
             return
          else
             if (trim(header) /= trim(test_head)) then
                BACKSPACE(41)
                write(41,'(a)') ''
             else
                BACKSPACE(41)
                write(41,'(a)') trim(test_head)
                write(41,'(a)') '='
                return
             end if
          end if
          exit
       else if(error==0) then 
          select case(part)
          case ('a')
             !write(*,*) partidx,'       ',stationidx
             if(partidx=="TTAA" .and. stationidx == 48698) then
                !write(*,*) partidx,'       ',stationidx
                sof=.true.
                backspace(31)
                read(31,'(a256)',iostat=error) line
                if(error/=0)then
                else
                   length=len_trim(line)
                   !write(*,*) line(length:)
                   if(line(length:).eq.'=') then
                      eof=.true.
                      write(41,'(a)') ''
                   else
                      write(41,'(a)') trim(line)
                   endif
                endif
             endif
          case ('c') 
             !write(*,*) partidx,'       ',stationidx
             if(partidx=="TTCC" .and. stationidx == 48698) then
                !write(*,*) partidx,'       ',stationidx
                sof=.true.
                backspace(31)
                read(31,'(a256)',iostat=error) line
                write(*,*) line
                if(error/=0)then
                else
                   length=len_trim(line)
                   write(*,*) line(length:)
                   if(line(length:).eq.'=') then
                      eof=.true.
                      BACKSPACE(41)
                      write(41,'(a)') ''
                   else
                      BACKSPACE(41)
                      write(41,'(a)') trim(line)
                   endif
                endif
             endif
          case ('d')
             if(partidx=="TTDD" .and. stationidx == 48698) then
                !write(*,*) partidx,'       ',stationidx
                sof=.true.
                backspace 31
                read(31,'(a256)',iostat=error) line
                if(error/=0)then
                else
                   length=len_trim(line)
                   !write(*,*) line(length:)
                   if(line(length:).eq.'=') then
                      eof=.true.
                      BACKSPACE(41)
                      write(41,'(a)') ''
                   else
                      BACKSPACE(41)
                      write(41,'(a)') trim(line)
                   endif
                endif
             endif
          end select
       else 
          !write(*,*) 'next line'
          !write(*,*) 'end of file'
          !eof=.true			
       endif
    end do

    do while(.not. eof)
       read(31,'(a256)',iostat=error) line
       !write(*,*) line
       if(error/=0) then
          write(*,*) 'error: ',error
       else	
          length=len_trim(line)
          !write(*,*) line(length:)
          if(line(length:).eq.'=') then
             !write(*,*) 'end of ',part
             eof=.true.
          else
             write(41,'(a)') adjustl(trim(line))
          endif
       endif
    end do
    backspace 41
    read(41,'(a256)',iostat=error) line
    !write(*,*) trim(outputfile(len_trim(outputfile)-12:))
    !write(*,*) 'sing_gen(line): ',trim(line)
    if(error/=0)then
    else
       line=trim(line)//'='
       backspace 41
       write(41,'(a)') adjustl(trim(line))
    endif

  end subroutine copy

end program gen_indata
