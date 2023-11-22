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
PROGRAM readqbo

  USE mod_read_qbo,   ONLY : rqbo,      &
                             rqbo_sing, &
                             rshea,     &
                             read_100
  USE mod_monthcount, ONLY : monthcount
  USE kind_mod,       ONLY : sp

  IMPLICIT NONE

  INTEGER             :: i, ierr, nrec 
  INTEGER             :: month, year
  INTEGER             :: isyear, ieyear, ny
  INTEGER             :: ismonth, iemonth
  INTEGER             :: i100_sh
  INTEGER             :: isy_100hpa
  REAL(sp), PARAMETER :: missing=-99999. 
  INTEGER,  PARAMETER :: lun=40          ! logical unit number of the input
  INTEGER,  PARAMETER :: nmonth=12, nyears=74
  INTEGER,  PARAMETER :: nlev_sh=15, nlev_nj=7 ! number levels in input
  INTEGER,  PARAMETER :: nlev_si=15
  INTEGER,  PARAMETER :: nlev=8                ! number of output levels
  INTEGER             :: levels_sh(nlev_sh)    ! press. levels shea
  INTEGER             :: levels_nj(nlev_nj)    ! press. levels naujokat
  INTEGER             :: levels_si(nlev_si)    ! press. levels singapore
  INTEGER             :: levels(nlev)          ! press. levels output
  REAL(sp)            :: rdata_nj(nlev_nj,nmonth,nyears)
  REAL(sp)            :: rdata_sh(nlev_sh,nmonth,nyears)
  REAL(sp)            :: rdata_si(nlev_si,nmonth,nyears)
  REAL(sp)            :: rdata_100(nmonth,nyears)
  REAL(sp)            :: rdata(nlev,nmonth,nyears) ! output data
  CHARACTER(len=8), PARAMETER :: sdate_100hpa='19970101'
  CHARACTER(len=4)    :: yymm 
  CHARACTER(len=6)    :: sdate, edate
  CHARACTER(len=60)   :: header 
  CHARACTER(len=256)  :: infile, infile_si, infile_sh, infile_100, outfile

  NAMELIST /INPUT/ isyear, ieyear, infile, infile_si, infile_sh, infile_100, outfile

  !     --> open the data file, with the QBO data:                        
  !     ------------------------------------------                        
  infile    = 'qbo_53-01.dat' 
  infile_si = 'singapore_87-01.dat'
  infile_sh = '/home/kunze/data/qbo/shea/QBO/qbo_u'
  infile_100 = 'qbo_100hPa.dat'
  
  levels_nj = (/ 70, 50, 40, 30, 20, 15, 10 /)
  levels_si = (/ 100, 90, 80, 70, 60, 50, 45, 40, 35, 30, 25, 20, 15, 12, 10 /)
  levels_sh = (/1000,850,700,500,400,300,250,200,150,100, &
       &          70, 50, 30, 20, 10/)
  levels    = (/ 100, 70, 50, 40, 30, 20, 15, 10 /)
  
  i100_sh = 10
  
  isyear = 1980
  ieyear = 1999
  ismonth = 1
  iemonth = 12

  READ (2, NML = input)
  WRITE(*, NML = input)

  ny = ieyear - isyear + 1
  WRITE(sdate,"(i4.4,i2.2)") isyear,ismonth
  WRITE(edate,"(i4.4,i2.2)") ieyear,iemonth
  
  isy_100hpa = monthcount(sdate, sdate_100hpa) - 1
  isy_100hpa = INT(REAL(isy_100hpa,sp) / 12._sp) + 1
  IF (isy_100hpa < 1) isy_100hpa = 1

  print *,' sdate:',sdate,' edate:',edate,' isy_100hpa',isy_100hpa
  
  rdata_nj  = missing
  rdata_si  = missing
  rdata_sh  = missing
  rdata_100 = missing
  CALL rqbo     (infile, sdate, edate, lun, missing, rdata_nj)
  CALL rqbo_sing(infile_si, sdate, edate, lun, missing, rdata_si)
  CALL rshea    (infile_sh, sdate, edate, lun, missing, rdata_sh)
  CALL read_100 (missing, infile_100, sdate, edate, lun, rdata_100)
  
  rdata(2:nlev,:,1:ny) = rdata_nj(:,:,1:ny)
  rdata(1,:,1:ny)      = rdata_sh(i100_sh,:,1:ny)
  rdata(1,:,isy_100hpa:ny) = rdata_100(:,isy_100hpa:ny)
  !     --> write a GrADS binary file:                                    
  !     ------------------------------                                    
  OPEN (30, file = TRIM(outfile)//'.gra', access = 'direct', recl = nlev*4) 
  nrec = 0 
  DO year = 1, ny
     DO month = 1, nmonth 
        nrec = nrec + 1 
        WRITE (30, rec = nrec) rdata(:,month,year)
     END DO
  END DO

  !     --> write a GrADS control file:                                   
  !     -------------------------------                                   
  OPEN  (30, file = TRIM(outfile)//'.ctl') 
  WRITE (30,'(a)')         'dset ^'//TRIM(outfile)//'.gra'
#if defined(BIG_ENDIAN)
  WRITE (30,'(a)')         'options big_endian'
#endif
#if defined(LITTLE_ENDIAN)
  WRITE (30,'(a)')         'options little_endian'
#endif
  WRITE (30,'(a)')         'title Monthly mean zonal '//    &
       &                       'wind components u (m/s) '             
  WRITE (30,'(a6,f7.0)')   'undef ',missing
  WRITE (30,'(a)')         'xdef 1 linear 0 1' 
  WRITE (30,'(a)')         'ydef 1 linear 0 1'
PRINT *,':levels:',levels
  WRITE (30,'(a,i2,a,8i6)')'zdef ',nlev,' levels ',levels
  WRITE (30,'(a,i3,a)')    'tdef ',ny*nmonth,' linear '//    &
       &                         '01jan'//sdate(1:4)//' 1mo' 
  WRITE (30,'(a)')         'vars 1' 
  WRITE (30,'(a,i2,a)')    'u ',nlev,' 99 wind component u (m/s)' 
  WRITE (30,'(a)')         'endvars' 
  ! -
END PROGRAM readqbo
