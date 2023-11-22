!     ------------------------------------------------------------------
!     PROGRAM READQBO:
!     ----------------
!     - read monthly mean zonal wind components u from 
!       data file qbo_53-01.dat
!     - the data file contains data from the stations:
!       CANTON ISLAND  02 46 S  171 43 W
!       GAN/MALEDIVES  00 41 S   73 09 E
!       SINGAPORE      01 22 N  103 55 E
!     - integer value behind the wind data gives informatopn about:
!       NN=1-9 if less than 10 daily values, =99 interpolated values
!     
!     Stratospheric Research Group FU-Berlin
!     ------------------------------------------------------------------
      PROGRAM READQBO

      implicit none

      integer             i, ierr, nrec
      integer             month, year, station, ndel
      integer, parameter :: nmonth=12, nyears=50, nlev=7
      integer, parameter :: missing=-99999
      integer, dimension(nlev)               :: nn
      integer, dimension(nlev,nmonth,nyears) :: qbo
      character(len=4)                       :: yymm
      character(len=60)                      :: header
      character(len=256)                     :: infile
    
!     --> open the data file, with the QBO data:
!     ------------------------------------------
      infile = '/opt/data/cdrom/data/qbo/qbo_53-01.dat'
      
      open (79, file = infile, iostat = ierr)
      if (ierr .ne. 0) then
         print *, 'NOT open data file ',infile
         stop
      end if
      
!     ignore header data until startdate:
!     -----------------------------------
      do i = 1, 9
         read(79,'(a54)') header
      end do

 100  format( i5, 1x, a4, i1, 7(i5,i2) )
      do year = 1, nyears
         do month = 1, nmonth
            read(79, 100) station, yymm, ndel, &
                 &      ( qbo(i,month,year), nn(i), i = 1, nlev )
            do i = 1, nlev
               if ( qbo(i,month,year) .eq. 0)  &
                    &  qbo(i,month,year) = missing
            end do
         end do
      end do
      close(79)

!     --> write a GrADS binary file:
!     ------------------------------
      open ( 30, file = 'qbo.dat', access = 'direct', recl = nlev*4)
      nrec = 0
      do year = 1, nyears
         do month = 1, nmonth
            nrec = nrec + 1
            write ( 30, rec = nrec) ( qbo(i,month,year)*1.,i = 1, nlev )
         end do
      end do

!     --> write a GrADS control file:
!     -------------------------------
      open  (30, file = 'qbo.ctl')
      write (30,'(a)')       'dset ^qbo.dat'
      write (30,'(a)')       'title Monthly mean zonal '// &
           &                 'wind components u (0.1 m/s) '
      write (30,'(a6,f7.0)') 'undef ',float(missing)
      write (30,'(a)')       'xdef 1 linear 0 1'
      write (30,'(a)')       'ydef 1 linear 0 1'
      write (30,'(a)')       'zdef 7 levels 70 50 40 30 20 15 10'
      write (30,'(a,i3,a)')  'tdef ',nyears*nmonth,' linear '// &
           &                 '01jan1953 1mo'
      write (30,'(a)')       'vars 1'
      write (30,'(a)')       'u 7 99 wind component u (0.1 m/s)'
      write (30,'(a)')       'endvars'
      
      END
