!     ------------------------------------------------------------------
!     PROGRAM READ_QBO:
!     -----------------------
!     - read monthly mean zonal wind components u from 
!       data file singapore_87-01.dat
!     - the data file contains data from the station:
!       SINGAPORE      01 22 N  103 55 E
!     
!     Stratospheric Research Group FU-Berlin
!     ------------------------------------------------------------------
PROGRAM READSING
  
  USE read_qbo_data
  USE mod_read_qbo,   ONLY : rshea
  USE mod_monthcount, ONLY : monthcount

  implicit none

  INTEGER                             :: i, ierr, nrec, yyyy
  INTEGER                             :: month, year, lev, nyears
  INTEGER                             :: syyyy=1987, eyyyy=2005
  INTEGER                             :: ismonth, iemonth
  INTEGER                             :: isy_100hpa
  INTEGER, PARAMETER                  :: i100_sh = 10
  INTEGER, PARAMETER                  :: lun = 88
  INTEGER, PARAMETER                  :: nlev_sh=15
  INTEGER, PARAMETER                  :: nmonth=12, nlev=15
  real,    PARAMETER                  :: missing=-99999.
  real, dimension(:,:,:), allocatable :: qbo
  real, dimension(:,:,:), allocatable :: rdata_sh
  CHARACTER(len=8), PARAMETER     :: sdate_100hpa='19970101'
  CHARACTER(len=6)                :: sdate, edate
  CHARACTER(len=3)                :: level
  CHARACTER(len=60)               :: header
  CHARACTER(len=256)              :: infile1, infile2, infile3, outfile
  
  namelist /INP/ syyyy, eyyyy, infile1, infile2, infile3, outfile

  outfile = 'qbo_all'
  
  !  --> the data files, with the QBO data:
  !  ------------------------------------------
  infile1 = '/home/aktuell/data/qbo/data/shea/QBO/qbo_u'
  infile2 = '/home/aktuell/data/qbo/data/qbo.highres.dat'
  infile3 = '/home/aktuell/data/qbo/data/qbo_100hPa.dat'

  read (12,NML=INP)
  write(*,NML=INP)

  ismonth = 1
  iemonth = 12
  nyears  = eyyyy - syyyy + 1
  WRITE(sdate,"(i4.4,i2.2)") syyyy,ismonth
  WRITE(edate,"(i4.4,i2.2)") eyyyy,iemonth
  isy_100hpa = monthcount(sdate, sdate_100hpa) - 1
  isy_100hpa = INT(REAL(isy_100hpa) / 12.) + 1

  !  --> allocate data array:
  !  ------------------------
  allocate( qbo(nlev,nmonth,nyears), &
            rdata_sh(nlev_sh,nmonth,nyears), stat = ierr)
  if (ierr /= 0) then
     print *,'--> qbo allocation failed!'
     call exit(1)
  end if

  !  --> open the GrADS binary output file:
  !  --------------------------------------

  open ( 30, file = trim(outfile)//'.gra',   &
       &   access = 'DIRECT',  &
       &   status = 'REPLACE', &
       &     recl = nlev*4, iostat = ierr)
  if (ierr /= 0) then
     print *, 'NOT open data file: '//trim(outfile)
     stop
  end if
  
  qbo = missing
  CALL read_highres(missing, infile2, syyyy, qbo)
  CALL read_100    (missing, infile3, syyyy, qbo(nlev,:,:))
  CALL rshea       (infile1, sdate, edate, lun, missing, rdata_sh)
  WHERE ( qbo(nlev,:,:) == missing )
     qbo(nlev,:,:) = rdata_sh(i100_sh,:,:)
  END WHERE
  
  !  --> write the GrADS binary file:
  !  --------------------------------
  nrec = 0
  do year = 1, nyears
     do month = 1, nmonth
        nrec = nrec + 1
        write ( 30, rec = nrec)  &
             & ( qbo(i,month, year)*1.,i = nlev, 1, -1 )
     end do
  end do

  !  --> write a GrADS control file:
  !  -------------------------------
  open  (30, file = trim(outfile)//'.ctl')
  write (30,'(a)')       'dset ^'//trim(outfile)//'.gra'
  write (30,'(a)')       'title Monthly mean zonal '// &
       &                 'wind components u (0.1 m/s) '
  write (30,'(a6,f7.0)') 'undef ',missing
  write (30,'(a)')       'xdef 1 linear 0 1'
  write (30,'(a)')       'ydef 1 linear 0 1'
  write (30,'(a)')       'zdef 15 levels '
  write (30,'(a)')       '    100 90 80 70 60'
  write (30,'(a)')       '     50 45 40 35 30'
  write (30,'(a)')       '     25 20 15 12 10'
  write (30,'(a,i3,a)')  'tdef ',nyears*nmonth,' linear '// &
       &                 '01jan1987 1mo'
  write (30,'(a)')       'vars 1'
  write (30,'(a)')       'u 15 99 wind component u (0.1 m/s)'
  write (30,'(a)')       'endvars'

END PROGRAM READSING
